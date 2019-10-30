(ns matr-core.actions
  (:require
   [datascript.core :as d]
   [schema.core :as schema]
   [taoensso.timbre :as timbre :refer [log spy]]
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
                                        :matr.node/flags ["checked"]
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

(defn all-newsyms [db newsyms antecedents]
  (letfn [(newsym-name [sym]
            (if (string? sym)
              sym
              (:name sym)))
          (coerce-newsym [sym]
            (if (string? sym)
              {:matr/kind :matr.kind/symbol
               :matr.symbol/name sym}
              {:matr/kind :matr.kind/symbol
               :matr.symbol/name (:name sym)
               :matr.symbol/type (:type sym)}))]
    (let [newsyms (->> antecedents
                       (map :newsyms)
                       (cons newsyms)
                       (apply concat))]
      (assert (not (seq (d/q '[:find ?s :in $ [?name ...] :where
                               [?s :matr/kind :matr.kind/symbol]
                               [?s :matr.symbol/name ?name]]
                             db
                             (map newsym-name newsyms))))
              "Some syms already existed")
      (->> newsyms
           (map coerce-newsym)
           (into [])))))

(schema/defschema NewSym
  (schema/cond-pre schema/Str
                   {:name schema/Str
                    :type schema/Str}))

(schema/defschema JustificationAction
  {:action (schema/eq "add_justification")
   :box schema/Int
   :antecedents [(schema/cond-pre schema/Int
                                  {:formula schema/Str
                                   :newsyms [NewSym]
                                   :newaxioms [schema/Str]})]
   :consequence schema/Str
   :name schema/Str
   (schema/optional-key :local-id) schema/Any
   (schema/optional-key :newsyms) [NewSym]})

(defmethod action->datoms "add_justification"
  [db {boxid :box, :keys [antecedents consequence name local-id newsyms]}]
  (let [consequentIdMap (db-nodes-query db boxid [consequence])
        complex-antecedents (remove number? antecedents)
        antecedent-formula (->> complex-antecedents
                                (map :formula)
                                (into #{}))
        antecedent-ids (->> antecedents
                            (filter number?)
                            (into #{}))
        justification (d/q db-justification-query db boxid name antecedent-ids antecedent-formula consequence)]
    (when-not justification
      (let [anteceedentIdMap (db-nodes-query db boxid complex-antecedents)
            consequence (or (consequentIdMap consequence)
                            {:matr/kind :matr.kind/node
                             :db/id consequence
                             :matr.node/formula consequence
                             :matr.node/parent boxid})
            newsyms (all-newsyms db newsyms antecedents)
            antecedents (process-justification-anteceedents db boxid complex-antecedents)]
        (conj newsyms
              {:db/id local-id
               :matr/kind :matr.kind/justification
               :matr.justification/inference-name name
               :matr.node/consequents consequence
               :matr.node/_consequents (concat antecedent-ids antecedents)
               :matr.node/parent boxid})))))

(defmethod action->datoms "add_box" [db action]
  nil)

(schema/defschema AddAxiomAction
  {:action (schema/eq "addAxiom")
   :formula schema/Str})

(defmethod action->datoms "addAxiom"
  [db {:keys [formula]}]
  (let [rootbox (db-rootbox-query db)]
    (if-let [eid (d/q '[:find ?e . :in $ ?f ?rootbox :where
                        [?e :matr.node/formula ?f]
                        [?e :matr.node/parent ?rootbox]]
                      db formula rootbox)]
      [{:db/id eid
        :matr.node/flags ["checked"]
        :matr.box/_axioms rootbox}]
      [{:matr/kind :matr.kind/node
        :matr.node/formula formula
        :matr.node/parent rootbox
        :matr.node/flags ["checked"]
        :matr.box/_axioms rootbox}])))

(schema/defschema AddGoalAction
  {:action (schema/eq "addGoal")
   :formula schema/Str})

(defmethod action->datoms "addGoal"
  [db {:keys [formula]}]
  (let [rootbox (db-rootbox-query db)]
    (if-let [eid (d/q '[:find ?e . :in $ ?f ?rootbox :where
                        [?e :matr.node/formula ?f]
                        [?e :matr.node/parent ?rootbox]]
                      db formula rootbox)]
      [{:db/id eid
        :matr.box/_goals rootbox}]
      [{:matr/kind :matr.kind/node
        :matr.node/formula formula
        :matr.node/parent rootbox
        :matr.box/_goals rootbox}])))

(schema/defschema FlagAction
  {:action (schema/enum "flag" "unflag")
   :box schema/Int
   :formula schema/Str
   :flag schema/Str})

(defmethod action->datoms "flag"
  [db {boxid :box, :keys [formula flag]}]
  (when-let [eid (d/q '[:find ?e . :in $ ?b ?f :where
                        [?e :matr.node/parent ?b]
                        [?e :matr.node/formula ?f]]
                      db boxid formula)]
    [[:db/add eid :matr.node/flags flag]]))

(defmethod action->datoms "unflag"
  [db {boxid :box :keys [formula flag]}]
  (when-let [eid (d/q '[:find ?e . :in $ ?b ?f :where
                        [?e :matr.node/parent ?b]
                        [?e :matr.node/formula ?f]]
                      db boxid formula)]
    [[:db/retract eid :matr.node/flags flag]]))

(schema/defschema FlagEntityAction
  {:action (schema/enum "flagEntity" "unflagEntity")
   :eid schema/Int
   :flag schema/Str})

(defmethod action->datoms "flagEntity"
  [db {:keys [eid flag]}]
  [[:db/add eid :matr.node/flags flag]])

(defmethod action->datoms "unflagEntity"
  [db {:keys [eid flag]}]
  [[:db/retract eid :matr.node/flags flag]])

(schema/defschema AddSymbolAction
  {:action (schema/eq "add_symbol")
   :name schema/Str
   :type schema/Str})

(defmethod action->datoms "add_symbol"
  [db {:keys [name type]}]
  (when-not (d/entity db [:matr.symbol/name name])
    [{:matr/kind :matr.kind/symbol
      :matr.symbol/name name
      :matr.symbol/type type}]))

(defn actions->datoms [db actions]
  (->>
   actions
   (map #(action->datoms db %))
   (apply concat)
   (into [])))

(defn actions->transaction
  "Generate a simple transaction which runs each of the actions and
  tags itself with action-name."
  ([actions] [[:db.fn/call #'matr-core.actions/actions->datoms actions]])
  ([action-name actions]
   [[:db.fn/call #'matr-core.actions/actions->datoms actions]
    {:db/id :db/current-tx :matr.tx/action-name action-name}]))

(schema/defschema Action
  (schema/conditional
   (comp #(= "add_justification" %) :action) JustificationAction
   (comp #(= "addAxiom" %) :action) AddAxiomAction
   (comp #(= "addGoal" %) :action) AddGoalAction
   (comp #{"flag" "unflag"} :action) FlagAction
   (comp #{"flagEntity" "unflagEntity"} :action) FlagEntityAction
   (comp #(= "add_symbol" %) :action) AddSymbolAction))
