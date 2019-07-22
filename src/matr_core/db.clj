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
                         :db/cardinality :db.cardinality/one}})

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
