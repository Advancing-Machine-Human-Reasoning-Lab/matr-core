(ns matr-core.codelets
  (:require
   [clojure.core.async :as a]
   [clojure.edn :as edn]
   [ajax.core :as ajax]
   [datascript.core :as d]
   [schema.core :as schema]
   [taoensso.timbre :as timbre :refer [log spy]]
   [matr-core.utils :refer [juxt-map db-since]]
   [matr-core.db :refer [schema conn db-codelets-query db-justification-query transact!]]
   [matr-core.actions :refer [actions->transaction Action]]))

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
  (->> resp
       (schema/validate [[Action]])
       (map actions->transaction)))

(defn find-justifications-to-reiterate
  "Given a sequence of node ids, return the transaction information
  required to reiterate applicable justifications from their parent
  boxes."
  [db nodes]
  (let [justifications (d/q '[:find ?n ?f ?b ?bp ?j :in $ % [?n ...] :where
                              [?n :matr.node/formula ?f] [?n :matr.node/parent ?b]
                              (ancestor ?bp ?b)
                              [?j :matr.node/parent ?bp] [?j :matr/kind :matr.kind/justification]
                              (not [?j :matr.node/inference-name "Reiteration"])
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

(def errors (atom []))
(def error-queries
  '[[:find ?p ?q ?f ?b :where
     [?p :matr.node/formula ?f]
     [?q :matr.node/formula ?f]
     [(not= ?p ?q)]
     [?p :matr.node/parent ?b]
     [?q :matr.node/parent ?b]]])

(defn handle-codelet-response
  "Convert and run the actions returned by the codelet server."
  [codelet iteration-tx chan]
  (fn [resp]
    (try
      (doseq [transaction (codelet-response->transactions resp)]
        (let [tx
              (transact! conn (concat (spy :debug transaction)
                                      [[:db/add (:db/id codelet) :matr.codelet/transaction-since iteration-tx]]))]
          (doseq [q error-queries]
            (when (and (not (seq (d/q q (:db-before tx)))) (seq (d/q q (:db-after tx))))
              (swap! errors conj [q transaction tx])))))
      (a/go (a/>! chan :done))
      (catch Exception e
        (log :debug e)))))

(defn reiterate-justifications
  "Reiterate any justifications from parent boxes applicable to the given nodes."
  [nodes]
  (transact! conn (find-justifications-to-reiterate @conn nodes)))

(defn find-unchecked-nodes 
  [uncheckedNodes]
  (let [curAxioms (set (d/q '[:find [?a ...] :where [?n :matr/kind :matr.kind/box] [?n :matr.box/axioms ?a]] @conn))]
    (doseq [x (vec (rseq (vec (sort uncheckedNodes))))]  
                                        ; iterate over the justifications of x 
      (doseq [y (d/q '[:find [?ant ...] :in $ ?cons :where 
                       [?ant :matr/kind :matr.kind/justification] 
                       [?ant :matr.node/consequents ?cons]] @conn x)]
                                        ; find antecs nodes of justification that are not checked 
        (let [antecs (d/q '[:find [?n ...] :in $ ?j :where 
                            [?n :matr.node/consequents ?j]
                            (not [?n :matr.node/flags "checked"])] @conn y)]
                                        ; when difference of {unchecked antecs of just y} and {axioms} is empty
          (when (= #{} (clojure.set/difference (set antecs) curAxioms))
                                        ; when set of all checked nodes does not contain node x
            (when (not (contains? (set (d/q '[:find [?n ...] :where 
                                              [?n :matr/kind :matr.kind/node] 
                                              [?n :matr.node/flags "checked"]] 
                                            @conn)) x))
              (transact! conn [[:db/add x :matr.node/flags "checked"]]))))))))

(defn async-all [chans]
  (a/go-loop [chanset (set chans) res {}]
    (if-let [chan-seq (seq chanset)]
      (let [[val chan] (a/alts! chan-seq)]
        (recur (disj chanset chan) (assoc res chan val)))
      (map res chans))))

(defn step-proofer
  "Explore unexplored nodes by sending them to the codelets server and
  reiterating justifications applicable to them."
  ([] (step-proofer ["ALL"] (d/q '[:find [?n ...] :where
                                   [?n :matr/kind :matr.kind/node]
                                   (not [?n :matr.node/flags "explored"])]
                                 @conn)))
  ([nodes] (step-proofer ["ALL"] nodes))
  ([codelets nodes]
   (let [{db :db-after {tx :db/current-tx} :tempids}
         (transact! conn [{:db/id :db/current-tx :matr.tx/codelet-checkpoint true}])]
     (doseq [[stage codelets] (->> (d/q db-codelets-query db)
                                   (reduce (fn [m [k v]]
                                             (update m k (fnil conj []) v))
                                           (sorted-map)))]
       (let [db @conn] ; Each stage needs to be able to see the changes since the previous stage
         (->> (for [codelet codelets]
                (let [res (if (:matr.codelet/query-include-since codelet)
                            (d/q (edn/read-string (:matr.codelet/query codelet))
                                 db
                                 (db-since db (:matr.codelet/transaction-since codelet)))
                            (d/q (edn/read-string (:matr.codelet/query codelet)) db))
                      chan (a/chan)]
                  (when (if (seqable? res) (seq res) res)
                    (ajax/POST (:matr.codelet/endpoint codelet)
                      {:format :json
                       :response-format :json
                       :keywords? true
                       :params res
                       :handler (handle-codelet-response codelet tx chan)})
                    chan)))
              (filter identity)
              doall
              async-all
              a/<!!))))))
