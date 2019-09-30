(ns matr-core.actions
  (:require
   [datascript.core :as d]
   [matr-core.db :refer [db-rootbox-query db-nodes-query run-db-box-from-axioms-query
                         db-justification-query pull-all-axioms]]))

(defmulti action->datoms
  "Convert a map describing a single action to the proper datoms required to implement that action"
  (fn [db action]
    (or (get action :actions)
        (get action :action))))

(defn actually-new-axioms [db boxid new-axioms]
  (let [new-axioms (set new-axioms)]
    (clojure.set/difference new-axioms (pull-all-axioms db boxid))))

(defn process-justification-anteceedent [db boxid anteceedent anteceedent-nodes-map]
  (let [{:keys [newsyms newaxioms formula]} anteceedent
        new-axioms (actually-new-axioms db boxid newaxioms)]
    (if (seq new-axioms)
      (if-let [b (run-db-box-from-axioms-query db boxid newaxioms)]
        {:matr/kind :matr.kind/node
         :matr.node/parent b
         :matr.node/formula formula}
        {:matr/kind :matr.kind/box
         :matr.node/_parent
         (into [] (concat (map (fn [f] {:db/id f :matr.node/formula f
                                        :matr/kind :matr.kind/node})
                               new-axioms)
                          [{:matr/kind :matr.kind/node
                            :db/id formula :matr.node/formula formula}]))
         :matr.box/axioms (into [] new-axioms)
         :matr.box/parent boxid})
      (or (anteceedent-nodes-map formula)
          {:matr/kind :matr.kind/node
           :matr.node/formula formula
           :matr.node/parent boxid}))))

(defn process-justification-anteceedents [db boxid anteceedents]
  (let [anteceedent-nodes-map (->> anteceedents
                                   (map #(get % :formula))
                                   (into [])
                                   (db-nodes-query db boxid))]
    (for [anteceedent anteceedents]
      (process-justification-anteceedent db boxid anteceedent anteceedent-nodes-map))))

(defmethod action->datoms "add_justification" [db action]
  (let [{boxid :box, :keys [antecedents consequence name local-id]} action
        consequentIdMap (db-nodes-query db boxid [consequence])
        antecedent-formula (->> antecedents
                                (map #(get % :formula))
                                (into #{}))
        justification (d/q db-justification-query db boxid name antecedent-formula consequence)]
    (when-not justification
      (let [anteceedentIdMap (db-nodes-query db boxid antecedents)
            consequence (or (consequentIdMap consequence)
                            {:matr/kind :matr.kind/node
                             :db/id consequence
                             :matr.node/formula consequence
                             :matr.node/parent boxid})
            antecedents (process-justification-anteceedents db boxid antecedents)]
        {:db/id local-id
         :matr/kind :matr.kind/justification
         :matr.justification/inference-name name
         :matr.node/consequents consequence
         :matr.node/_consequents antecedents
         :matr.node/parent boxid}))))

(defmethod action->datoms "add_box" [db action]
  nil)

(defmethod action->datoms "addAxiom" [db action]
  (let [rootbox (db-rootbox-query db)
        formula (get action :formula)]
    (if-let [eid (d/q '[:find ?e . :in $ ?f ?rootbox :where
                        [?e :matr.node/formula ?f]
                        [?e :matr.node/parent ?rootbox]]
                      db formula rootbox)]
      {:db/id eid
       :matr.node/flags ["checked"]
       :matr.box/_axioms rootbox}
      {:matr/kind :matr.kind/node
       :matr.node/formula formula
       :matr.node/parent rootbox
       :matr.node/flags ["checked"]
       :matr.box/_axioms rootbox})))

(defmethod action->datoms "addGoal" [db action]
  (let [rootbox (db-rootbox-query db)
        formula (get action :formula)]
    (if-let [eid (d/q '[:find ?e . :in $ ?f ?rootbox :where
                        [?e :matr.node/formula ?f]
                        [?e :matr.node/parent ?rootbox]]
                      db formula rootbox)]
      {:db/id eid
       :matr.box/_goals rootbox}
      {:matr/kind :matr.kind/node
       :matr.node/formula formula
       :matr.node/parent rootbox
       :matr.box/_goals rootbox})))

(defmethod action->datoms "flag" [db action]
  (let [{:keys [boxid formula flag]} action]
    (when-let [eid (d/q '[:find ?e . :in $ ?b ?f :where
                          [?e :matr.node/parent ?b]
                          [?e :matr.node/formula ?f]]
                        db boxid formula)]
      {:db/id eid
       :matr.node/flags [flag]})))

(defmethod action->datoms "unflag" [db action]
  (let [{:keys [boxid formula flag]} action]
    (when-let [eid (d/q '[:find ?e . :in $ ?b ?f :where
                          [?e :matr.node/parent ?b]
                          [?e :matr.node/formula ?f]]
                        db boxid formula)]
      [:db/retract eid :matr.node/flags flag])))

(defn actions->datoms [db actions]
  (->>
   actions
   (map #(action->datoms db %))
   (remove nil?)
   (into [])))

(defn actions->transaction
  "Generate a simple transaction which runs each of the actions and
  tags itself with action-name."
  ([actions] [[:db.fn/call #'matr-core.actions/actions->datoms actions]])
  ([action-name actions]
   [[:db.fn/call #'matr-core.actions/actions->datoms actions]
    {:db/id :db/current-tx :matr.tx/action-name action-name}]))
