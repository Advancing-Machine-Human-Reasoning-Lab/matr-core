(ns matr-core.db
  (:require [datascript.core :as d]
            [taoensso.timbre :as timbre :refer [log spy debugf]]))

(def schema {:matr.box/axioms {:db/type :db.type/ref
                               :db/cardinality :db.cardinality/many}
             :matr.box/goals {:db/type :db.type/ref
                              :db/cardinality :db.cardinality/many}
             :matr.box/parent {:db/type :db.type/ref
                               :db/cardinality :db.cardinality/one}
             :matr.box/logic {:db/type :db.type/string
                              :db/cardinality :db.cardinality/one}
             :matr.node/parent {:db/type :db.type/ref
                                :db/cardinality :db.cardinality/one}
             :matr.node/consequents {:db/type :db.type/ref
                                     :db/cardinality :db.cardinality/many}
             :matr.node/formula {:db/type :db.type/string
                                 :db/cardinality :db.cardinality/one}
             :matr.node/flags {:db/type :db.type/string
                               :db/cardinality :db.cardinality/many}
             :matr.justification/inference-name {:db/type :db.type/string
                                                 :db/cardinality :db.cardinality/one}
             :matr/kind {:db/type :db.type/keyword
                         :db/cardinality :db.cardinality/one}
             :matr.justification/logic {:db/cardinality :db.cardinality/one}
             :matr.codelet/endpoint {:db/type :db.type/string
                                     :db/cardinality :db.cardinality/one}
             :matr.codelet/query {:db/cardinality :db.cardinality/one}
             :matr.codelet/stage {:db/cardinality :db.cardinality/many}
             :matr.codelet/query-include-since {:db/type :db.type/boolean
                                                :db/cardinality :db.cardinality/one}
             :matr.codelet/last-query-tx {:db/type :db.type/ref
                                          :db/cardinality :db.cardinality/one}
             :matr.symbol/name {:db/cardinality :db.cardinality/one
                                :db/type :db.type/string
                                :db/unique :db.unique/identity}
             :matr.symbol/type {:db/type :db.type/string}
             :matr.tx/codelet-checkpoint {:db/cardinality :db.cardinality/one
                                          :db/type :db.type/boolean}})

(defn make-initial-db
  ([]
   (-> schema
       d/empty-db
       (d/db-with [{:matr/kind :matr.kind/box}])
       (agent :error-function (fn [a e] (log :debug e)))))
  ([logic]
   (-> schema
       d/empty-db
       (d/db-with [{:matr/kind :matr.kind/box :matr.box/logic logic}])
       (agent :error-function (fn [a e] (log :debug e))))))

(def conn (make-initial-db "ML"))

(defn transact! [conn datoms]
  (let [res (atom nil)]
    (letfn [(txer [db datoms]
              (try
                (let [tx (d/with db datoms)]
                  (reset! res [:good tx])
                  (:db-after tx))
                (catch Exception e
                  (reset! res [:bad e]))))]
      (send conn txer datoms)
      (await conn)
      (if-let [[quality value] @res]
        (case quality
          :good value
          :bad (throw value))
        (throw (Exception. "Lost a transaction response..."))))))

(def db-rootbox-query
  "Find the entity id of the box without any parent."
  (partial d/q '[:find ?rb . :in $ :where
                 [?rb :matr/kind :matr.kind/box]
                 [(missing? $ ?rb :matr.box/parent)]]))

(defn db-nodes-query
  "Find nodes in a given box by their formula.

  Returns a map from formula to node for nodes which were found in the
  box."
  [db boxid formulas]
  (->> (d/q '{:find [?formula ?e]
              :in [$ ?box [?formula ...]]
              :where [[?e :matr.node/parent ?box] [?e :matr.node/formula ?formula]]}
            db boxid formulas)
       (into {})))

(def db-box-query `[:find ?box . :in $ ?pbox ?logic ?ifs :where
                    [?box :matr.box/parent ?pbox]
                    [?box :matr.box/logic ?logic]
                    [(d/q [:find [?f ...] :in $ ?box
                           :where [?box :matr.box/axioms ?ax] [?ax :matr.node/formula ?f]]
                          $ ?box)
                     ?fl]
                    [(set ?fl) ?fs]
                    [(= ?fs ?ifs)]])

(defn run-db-box-query
  "Find sub-box of a given box with the given logic and axiom set."
  [db boxid logic axioms]
  (d/q db-box-query db boxid logic (set axioms)))

(def db-all-axioms-pull '[{:matr.box/axioms [:matr.node/formula] :matr.box/parent ...}])

(defn axioms-pull->axioms [axioms-pull]
  (when (seq axioms-pull)
    (concat (map :matr.node/formula (:matr.box/axioms axioms-pull))
            (axioms-pull->axioms (:matr.box/parent axioms-pull)))))

(defn pull-all-axioms [db boxid]
  (-> (d/pull db db-all-axioms-pull boxid)
      axioms-pull->axioms
      set))

(def db-justification-query '[:find ?sj .
                              :in $ ?box ?inference ?logic ?antecedent-nodes ?antecedent-formulas ?consequent-formula :where
                              [?c :matr.node/formula ?consequent-formula]
                              [?c :matr.node/parent ?box]
                              [?sj :matr.node/parent ?box]
                              [?sj :matr/kind :matr.kind/justification]
                              [?js :matr.justification/logic ?logic]
                              [?sj :matr.node/consequents ?c]
                              [?sj :matr.justification/inference-name ?inference]
                              [(datascript.core/q [:find [?saf ...] :in $ ?sj ?antecedent-nodes :where
                                                   [?s :matr.node/consequents ?sj]
                                                   [?s :matr.node/formula ?saf]
                                                   (not [(contains? ?antecedent-nodes ?s) ])]
                                                  $ ?sj ?antecedent-nodes)
                               ?afl]
                              [(set ?afl) ?antecedent-formulas]
                              [(datascript.core/q [:find [?s ...] :in $ ?sj ?antecedent-nodes :where
                                                   [?s :matr.node/consequents ?sj]
                                                   [(contains? ?antecedent-nodes ?s)]]
                                                  $ ?sj ?antecedent-nodes)
                               ?anl]
                              [(set ?anl) ?antecedent-nodes]])

(def db-codelets-query '[:find ?stage (pull ?c [:db/id :matr.codelet/endpoint
                                                :matr.codelet/query
                                                :matr.codelet/query-include-since
                                                :matr.codelet/transaction-since])
                         :where
                         [?c :matr/kind :matr.kind/codelet]
                         [?c :matr.codelet/stage ?stage]])
(defn propogate-distances-once [db nodes distances]
  (loop [justifs (d/q '[:find [(pull ?j [:db/id :matr.node/_consequents :matr.node/consequents]) ...] :in $ [?n ...]
                        :where [?n :matr.node/consequents ?j]]
                      db nodes)
         new-nodes #{}
         distances (transient distances)]
    (if-let [[{justif :db/id antecs :matr.node/_consequents [{conseq :db/id}] :matr.node/consequents} & justifs] (seq justifs)]
      (let [antec-distances (map (comp distances :db/id) antecs)
            conseq-distance (distances conseq)]
        (if (every? identity antec-distances)
          (let [new-distance (inc (reduce + 0 antec-distances))
                distances (assoc! distances justif new-distance)]
            (if (or (not conseq-distance) (< new-distance conseq-distance))
              (recur justifs (conj new-nodes conseq) (assoc! distances conseq new-distance))
              (recur justifs new-nodes distances)))
          (recur justifs new-nodes distances)))
      [new-nodes (persistent! distances)])))

(defn compute-distances [db]
  (loop [nodes-to-visit (d/q '[:find [?ax ...] :where [_ :matr.box/axioms ?ax]] db)
         distances (->> nodes-to-visit (map #(vector % 0)) (into {}))]
    (if-let [nodes-to-vist (seq nodes-to-visit)]
      (let [[new-nodes updated-distances] (propogate-distances-once db nodes-to-visit distances)]
        (recur new-nodes updated-distances))
      distances)))

(defn find-best-justif-for [db distances node]
  (let [js (d/q '[:find [?j ...] :in $ ?n :where [?j :matr.node/consequents ?n]] db node)
        dists (select-keys distances js)
        [best-j] (reduce (fn [[cj cd :as c] [nj nd :as n]] (if (< nd cd) n c)) dists)]
    (d/pull db [:db/id :matr.justification/inference-name :matr.node/_consequents] best-j)))

(defn find-proof-of [db distances node]
  (let [node (d/pull db [:db/id :matr.node/_consequents :matr.node/formula
                         :matr.node/parent :matr.box/_axioms :matr.box/goals]
                     node)]
    (if (or (seq (:matr.box/_axioms node)))
      #{(:db/id node)}
      (let [justifs (->> node :matr.node/_consequents (map :db/id) (select-keys distances))
            [best-j] (reduce (fn [[cj cd :as c] [nj nd :as n]] (if (< nd cd) n c)) justifs)
            justif (d/pull db [:db/id :matr.justification/inference-name
                               :matr.node/parent :matr.node/_consequents] best-j)]
        (->> justif
             :matr.node/_consequents
             (map (fn [n] (find-proof-of db distances (:db/id n))))
             (reduce clojure.set/union #{})
             (clojure.set/union #{(:db/id node) (:db/id justif)}))))))


(defn extract-proof-eids [db]
  (let [distances (compute-distances db)
        goals (d/q '[:find [?g ...] :where [1 :matr.box/goals ?g]] db)
        cgoals (d/q '[:find [?g ...] :where [1 :matr.box/goals ?g] [?g :matr.node/flags "checked"]] db)
        proofs (reduce clojure.set/union #{} (map #(find-proof-of db distances %) cgoals))
        boxes (conj (set (d/q '[:find [?b ...] :in $ [?n ...] :where [?n :matr.node/parent ?b]] db proofs)) 1)
        axioms (set (d/q '[:find [?a ...] :in $ [?b ...] :where [?b :matr.box/axioms ?a]] db boxes))]
    (clojure.set/union goals proofs boxes axioms)))
