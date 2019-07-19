(ns matr-core.codelets
  (:require
   [ajax.core :as ajax]
   [datascript.core :as d]
   [matr-core.utils :refer [juxt-map]]
   [matr-core.db :refer [schema conn]]
   [matr-core.actions :refer [actions->transaction]]))

;;;; Pure functions over db

(defn eids->codelet-nodereqs
  "Generate nodeReqs to send to codelets from a sequence of node ids."
  [db eids]
  (->> (d/pull-many db [:matr.node/formula :matr.node/source {:matr.node/parent [:db/id]}] eids)
       (map (juxt-map {"content" :matr.node/formula, "boxid" (comp :db/id :matr.node/parent)}))
       (into [])))


(defn codelet-response->transactions
  "Convert a response from the codelets server into a sequence of
  transactions."
  [resp]
  (let [transactions (->> (get resp "actionDetails")
                          (map first)
                          (map (fn [[action-name actions]]
                                 (actions->transaction action-name actions))))]
    transactions))

(defn find-justifications-to-reiterate
  "Given a sequence of node ids, return the transaction information
  required to reiterate applicable justifications from their parent
  boxes."
  [db nodes]
  (let [justifications (d/q '[:find ?n ?f ?b ?j :in $ % [?n ...] :where
                              [?n :matr.node/formula ?f] [?n :matr.node/parent ?b]
                              (ancestor ?bp ?b)
                              [?j :matr.node/parent ?bp] [?j :matr/kind :matr.kind/justification]
                              (not [?j0 :matr.node/parent ?b] [?j0 :matr.justification/reiterated-from ?j])
                              [?n0 :matr.node/consequents ?j] [?n0 :matr.node/formula ?f]
                              [?n1 :matr.node/consequents ?j]]
                            db
                            '[[(ancestor ?bp ?b) [?b :matr.box/parent ?bp]]
                              [(ancestor ?bp ?b) [?b :matr.box/parent ?b0] (ancestor ?bp ?b0)]]
                            nodes)]
    (->> (for [[n f b j] justifications]
           (let [antecedents (d/q '[:find [?a ...] :in $ ?j ?f :where
                                    [?a :matr.node/consequents ?j]
                                    (not [?a :matr.node/formula ?f])]
                                  db j f)
                 consequents (->> (d/q '[:find [?f ...] :in $ ?j :where
                                         [?j :matr.node/consequents ?c]
                                         [?c :matr.node/formula ?f]]
                                       db j b)
                                  (map (fn [f] (or (d/q '[:find ?n . :in $ ?f ?b :where
                                                          [?n :matr.node/parent ?b]
                                                          [?n :matr.node/formula ?f]]
                                                        db f b)
                                                   {:matr/kind :matr.kind/node
                                                    :matr.node/explored false
                                                    :matr.node/formula f
                                                    :matr.node/parent b})))
                                  (into []))]
             {:matr.justification/inference-name (d/q '[:find ?name . :in $ ?j :where
                                                        [?j :matr.justification/inference-name ?name]]
                                                      db j)
              :matr.justification/reiterated-from j
              :matr/kind :matr.kind/justification
              :matr.node/consequents consequents
              :matr.node/_consequents (into [] (cons n antecedents))
              :matr.node/parent b}))
         (into []))))

;;;; Side-effecting stuff

(defn handle-codelet-response
  "Convert and run the actions returned by the codelet server."
  [resp]
  (doseq [transaction (codelet-response->transactions resp)]
    (d/transact! conn transaction)))

(defn reiterate-justifications
  "Reiterate any justifications from parent boxes applicable to the given nodes."
  [nodes]
  (d/transact! conn (find-justifications-to-reiterate @conn nodes)))

(defn step-proofer
  "Explore unexplored nodes by sending them to the codelets server and
  reiterating justifications applicable to them."
  ([] (step-proofer ["ALL"] (d/q '[:find [?n ...] :where [?n :matr.node/explored false]] @conn)))
  ([nodes] (step-proofer ["ALL"] nodes))
  ([codelets nodes]
   @(ajax/POST "http://localhost:5002/callCodelets"
               {:format :json
                :params {'codeletlist codelets
                         'nodeReq (eids->codelet-nodereqs @conn nodes)}
                :handler handle-codelet-response})
   (reiterate-justifications nodes)
   (d/transact! conn (->> nodes (map #(vector :db/add % :matr.node/explored true)) (into [])))))
