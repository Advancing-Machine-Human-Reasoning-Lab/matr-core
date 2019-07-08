(ns matr-core.actions
  (:require
   [datascript.core :as d]
   [matr-core.db :refer [db-rootbox-query db-nodes-query run-db-box-from-axioms-query]]))

(defmulti action->datoms
  "Convert a map describing a single action to the proper datoms required to implement that action"
  (fn [db action]
    (or (get action "actions")
        (get action "action"))))

(defmethod action->datoms "addBox" [db action]
  (let [axioms (some-> action (get "axioms") (clojure.string/split #"-"))
        goals (some-> action (get "goals") (clojure.string/split #"-"))]
    {:db/id "newBox"
     :matr/kind :matr.kind/box
     :matr.node/_parent (into [] (concat (map (fn [f] {:db/id f :matr.node/formula f
                                                       :matr.node/explored false
                                                       :matr/kind :matr.kind/node
                                                       :matr.node/source "axioms"})
                                              axioms)
                                         (map (fn [f] {:db/id f :matr.node/formula f
                                                       :matr.node/explored false
                                                       :matr/kind :matr.kind/node
                                                       :matr.node/source "goals"})
                                              goals)))
     :matr.box/axioms axioms
     :matr.box/goals goals
     :matr.box/parent (get action "parentBoxid")}))


(defmethod action->datoms "addJustification" [db action]
  (let [boxid (get action "boxid")
        nextBox (some-> action (get "nextBox") (clojure.string/split #"-"))
        prevBox (some-> action (get "prevBox") (clojure.string/split #"-"))

        nextNode (some-> action (get "nextNode") (clojure.string/split #"-"))
        nextNodeIdMap (db-nodes-query db boxid nextNode)
        nextNodes (map (fn [f] (or (get nextNodeIdMap f) f)) nextNode)

        prevNode (some-> action (get "prevNode") (clojure.string/split #"-"))
        prevNodeIdMap (db-nodes-query db boxid prevNode)
        prevNodes (map (fn [f] (or (get prevNodeIdMap f) f)) prevNode)]
    {:matr.justification/inference-name (get action "nodeContent")
     :matr/kind :matr.kind/justification
     :matr.node/consequents (into [] (concat nextNodes nextBox))
     :matr.node/_consequents (into [] (concat prevNodes prevBox))
     :matr.node/parent boxid}))

(defmethod action->datoms "addNode" [db action]
  (if (d/q '[:find ?e . :in $ ?f ?b
             :where [?e :matr.node/formula ?f] [?e :matr.box/parent ?b]]
           db (get action "nodeContent") (get action "boxid"))
    nil
    {:matr/kind :matr.kind/node
     :db/id (get action "nodeContent")
     :matr.node/explored false
     :matr.node/formula (get action "nodeContent")
     :matr.node/source (get action "source")
     :matr.node/parent (get action "boxid")}))

(defmethod action->datoms "add_justification" [db action]
  (let [{boxid "box",antecedents "antecedents",
         consequence "consequence", name "name"} action
        consequentIdMap (db-nodes-query db boxid [consequence])
        anteceedentIdMap (->> antecedents
                              (map #(get % "formula"))
                              (into [])
                              (db-nodes-query db boxid))
        consequence (or (consequentIdMap consequence)
                        {:matr/kind :matr.kind/node
                         :db/id consequence
                         :matr.node/formula consequence
                         :matr.node/explored false
                         :matr.node/source "axioms"
                         :matr.node/parent boxid})
        antecedents (->> (for [{news "newsyms", newa "newaxioms", f "formula"} antecedents]
                           (or (anteceedentIdMap f)
                               (if-let [newa (seq newa)]
                                 (if-let [b (run-db-box-from-axioms-query db boxid newa)]
                                   {:db/id b
                                    :matr.node/_parent
                                    [{:matr/kind :matr.kind/node
                                      :matr.node/explored false
                                      :matr.node/formula f
                                      :matr.node/source "goals"}]}
                                   {:matr/kind :matr.kind/box
                                    :matr.node/_parent
                                    (into [] (concat (map (fn [f] {:db/id f :matr.node/formula f
                                                                   :matr.node/explored false
                                                                   :matr/kind :matr.kind/node
                                                                   :matr.node/source "axioms"})
                                                          newa)
                                                     [{:matr/kind :matr.kind/node
                                                       :matr.node/explored false
                                                       :matr.node/formula f
                                                       :matr.node/source "goals"}]))
                                    :matr.box/axioms (into [] newa)
                                    :matr.box/parent boxid})
                                 {:matr/kind :matr.kind/node
                                  :matr.node/explored false
                                  :matr.node/formula f
                                  :matr.node/source "goals"
                                  :matr.node/parent boxid})))
                         (into []))]
    {:matr/kind :matr.kind/justification
     :matr.justification/inference-name name
     :matr.node/consequents consequence
     :matr.node/_consequents antecedents
     :matr.node/parent boxid}))

(defmethod action->datoms "add_box" [db action]
  nil)

(defmethod action->datoms "addAxiom" [db action]
  (let [rootbox (db-rootbox-query db)
        formula (get action "formula")]
    (if-let [eid (d/q '[:find ?e . :in $ ?f ?rootbox :where
                        [?e :matr.node/formula ?f]
                        [?e :matr.node/parent ?rootbox]]
                      db formula rootbox)]
      {:db/id eid
       :matr.box/_axioms rootbox}
      {:matr/kind :matr.kind/node
       :matr.node/formula formula
       :matr.node/source "axioms"
       :matr.node/parent rootbox
       :matr.node/explored false
       :matr.box/_axioms rootbox})))

(defmethod action->datoms "addGoal" [db action]
  (let [rootbox (db-rootbox-query db)
        formula (get action "formula")]
    (if-let [eid (d/q '[:find ?e . :in $ ?f ?rootbox :where
                        [?e :matr.node/formula ?f]
                        [?e :matr.node/parent ?rootbox]]
                      db formula rootbox)]
      {:db/id eid
       :matr.box/_goals rootbox}
      {:matr/kind :matr.kind/node
       :matr.node/formula formula
       :matr.node/source "goals"
       :matr.node/parent rootbox
       :matr.node/explored false
       :matr.box/_goals rootbox})))


(defn actions->datoms [db actions]
  (->>
   actions
   (map #(action->datoms db %))
   (remove nil?)
   (into [])))

(defn actions->transaction
  "Generate a simple transaction which runs each of the actions and
  tags itself with action-name."
  [action-name actions]
  [[:db.fn/call #'matr-core.actions/actions->datoms actions]
   {:db/id :db/current-tx :matr.tx/action-name action-name}])
