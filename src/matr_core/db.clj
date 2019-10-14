(ns matr-core.db
  (:require [datascript.core :as d]))

(def schema {:matr.box/axioms {:db/type :db.type/ref
                               :db/cardinality :db.cardinality/many}
             :matr.box/goals {:db/type :db.type/ref
                              :db/cardinality :db.cardinality/many}
             :matr.box/parent {:db/type :db.type/ref
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
             :matr.justification/reiterated-from {:db/type :db.type/ref
                                                  :db/cardinality :db.cardinality/many}
             :matr/kind {:db/type :db.type/keyword
                         :db/cardinality :db.cardinality/one}
             :matr.codelet/endpoint {:db/type :db.type/string
                                     :db/cardinality :db.cardinality/one}
             :matr.codelet/query {:db/cardinality :db.cardinality/one}
             :matr.codelet/stage {:db/cardinality :db.cardinality/one}
             :matr.codelet/query-include-since {:db/type :db.type/boolean
                                                :db/cardinality :db.cardinality/one}
             :matr.codelet/last-query-tx {:db/type :db.type/ref
                                          :db/cardinality :db.cardinality/one}
             :matr.symbol/name {:db/cardinality :db.cardinality/one
                                :db/type :db.type/string
                                :db/unique :db.unique/identity}
             :matr.symbol/type {:db/type :db.type/string}})

(defn make-initial-db []
  (let [conn (d/create-conn schema)]
    (d/transact! conn [{:matr/kind :matr.kind/box}])
    conn))

(def conn (make-initial-db))

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

(def db-box-from-axioms-query `[:find ?box . :in $ ?pbox ?ifs :where
                                [?box :matr.box/parent ?pbox]
                                [(d/q [:find [?f ...] :in $ ?box
                                       :where [?box :matr.box/axioms ?ax] [?ax :matr.node/formula ?f]]
                                      $ ?box)
                                 ?fl]
                                [(set ?fl) ?fs]
                                [(= ?fs ?ifs)]])

(defn run-db-box-from-axioms-query
  "Find sub-boxes of a given box with the given axiom set."
  [db boxid axioms]
  (d/q db-box-from-axioms-query db boxid (set axioms)))

(def db-all-axioms-pull '[{:matr.box/axioms [:matr.node/formula] :matr.box/parent ...}])

(defn axioms-pull->axioms [axioms-pull]
  (when (seq axioms-pull)
    (concat (map :matr.node/formula (:matr.box/axioms axioms-pull))
            (axioms-pull->axioms (:matr.box/parent axioms-pull)))))

(defn pull-all-axioms [db boxid]
  (-> (d/pull db db-all-axioms-pull boxid)
      axioms-pull->axioms
      set))

(def db-justification-query '[:find ?sj . :in $ ?box ?inference ?antecedent-formulas ?consequent-formula :where
                              [?c :matr.node/formula ?consequent-formula]
                              [?c :matr.node/parent ?box]
                              [?sj :matr.node/parent ?box]
                              [?sj :matr/kind :matr.kind/justification]
                              [?sj :matr.node/consequents ?c]
                              [?sj :matr.justification/inference-name ?inference]
                              [(datascript.core/q [:find [?saf ...] :in $ ?sj :where
                                                   [?s :matr.node/consequents ?sj]
                                                   [?s :matr.node/formula ?saf]]
                                                  $ ?sj)
                               ?al]
                              [(set ?al) ?antecedent-formulas]])

(def db-codelets-query '[:find [(pull ?c [:db/id :matr.codelet/endpoint :matr.codelet/query :matr.codelet/stage]) ...] :where
                         [?c :matr/kind :matr.kind/codelet]])
