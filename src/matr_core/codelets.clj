(ns matr-core.codelets
  (:require
   [clojure.edn :as edn]
   [ajax.core :as ajax]
   [datascript.core :as d]
   [matr-core.utils :refer [juxt-map]]
   [matr-core.db :refer [schema conn db-codelets-query db-justification-query]]
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
  (let [justifications (d/q '[:find ?n ?f ?b ?bp ?j :in $ % [?n ...] :where
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
    (->> (for [[n f b pb j] justifications]
           (let [antecedents (d/q '[:find ?a ?af :in $ ?j ?f :where
                                    [?a :matr.node/consequents ?j]
                                    (not [?a :matr.node/formula ?f])
                                    [?a :matr.node/formula ?af]]
                                  db j f)
                 antecedent-formula (map second antecedents)
                 antecedents (for [[a af] antecedents]
                               {:matr/kind :matr.kind/node
                                :matr.node/formula af
                                :matr.node/parent b
                                :matr.node/_consequents
                                {:matr/kind :matr.kind/justification
                                 :matr.justification/inference-name "Reiteration"
                                 :matr.node/_consequents a
                                 :matr.node/parent pb}})
                 consequent-formula (d/q '[:find ?f . :in $ ?j :where
                                           [?j :matr.node/consequents ?c]
                                           [?c :matr.node/formula ?f]]
                                         db j)
                 inference (d/q '[:find ?name . :in $ ?j :where
                                  [?j :matr.justification/inference-name ?name]]
                                db j)
                 sub-just (d/q db-justification-query db b inference (into #{} (cons f antecedent-formula)) consequent-formula)]
             (when-not sub-just
               {:matr.justification/inference-name inference
                :matr.justification/reiterated-from j
                :matr/kind :matr.kind/justification
                :matr.node/consequents [(or (d/q '[:find ?n . :in $ ?f ?b :where
                                                   [?n :matr.node/parent ?b]
                                                   [?n :matr.node/formula ?f]]
                                                 db consequent-formula b)
                                            {:matr/kind :matr.kind/node
                                             :matr.node/flags ["explored"]
                                             :matr.node/formula consequent-formula
                                             :matr.node/parent b})]
                :matr.node/_consequents (into [] (cons n antecedents))
                :matr.node/parent b})))
         (filter identity)
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

(defn find-unchecked-nodes 
 [uncheckedNodes]
 (let [curAxioms (set (d/q '[:find [?a ...] :where [?n :matr/kind :matr.kind/box] [?n :matr.box/axioms ?a]] @conn))]
   (doseq [x (vec (rseq (vec (sort uncheckedNodes))))]  
     ; when x is not an axiom
     (when (not (contains? curAxioms x))
       ; iterate over the justifications of x 
       (doseq [y (d/q '[:find [?ant ...] :in $ ?cons :where 
                              [?ant :matr/kind :matr.kind/justification] 
                              [?ant :matr.node/consequents ?cons]] @conn x)]
        ; find antecs nodes of justification that are not checked 
         (let [antecs (d/q '[:find [?n ...] :in $ ?j :where 
                             [?n :matr/kind :matr.kind/node]
                             [?n :matr.node/consequents ?j]
                             (not [?n :matr.node/flags "checked"])] @conn y)]
          ; when difference of {unchecked antecs of just y} and {axioms} is empty
           (when (= #{} (clojure.set/difference (set antecs) curAxioms))
            ; when set of all checked nodes does not contain node x
             (when (not (contains? (set (d/q '[:find [?n ...] :where 
                                               [?n :matr/kind :matr.kind/node] 
                                               [?n :matr.node/flags "checked"]] 
                                          @conn)) x))
               (d/transact! conn [[:db/add x :matr.node/flags "checked"]])))))))))
  
(defn step-proofer
  "Explore unexplored nodes by sending them to the codelets server and
  reiterating justifications applicable to them."
  ([] (step-proofer ["ALL"] (d/q '[:find [?n ...] :where
                                   [?n :matr/kind :matr.kind/node]
                                   (not [?n :matr.node/flags "explored"])]
                                 @conn)))
  ([nodes] (step-proofer ["ALL"] nodes))
  ([codelets nodes]
   (let [db @conn]
     (->> (for [codelet (d/q db-codelets-query db)]
            (let [q (d/q (edn/read-string (:matr.codelet/query codelet)) db)]
              (when (seq q)
                (ajax/POST (:matr.codelet/endpoint codelet)
                           {:format :json
                            :params q
                            :handler handle-codelet-response}))))
          doall
          (map deref)
          doall))
   @(ajax/POST "http://localhost:5002/callCodelets"
               {:format :json
                :params {'codeletlist codelets
                         'nodeReq (eids->codelet-nodereqs @conn nodes)}
                :handler handle-codelet-response})
   (reiterate-justifications nodes)
   (find-unchecked-nodes (d/q '[:find [?n ...] :where [?n :matr/kind :matr.kind/node] (not [?n :matr.node/flags "checked"])] @conn))
   (d/transact! conn (->> nodes (map #(vector :db/add % :matr.node/flags "explored")) (into [])))))
