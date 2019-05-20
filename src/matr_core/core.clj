(ns matr-core.core
  (:require [clojure.edn :as edn]
            [clojure.core.async :as async]
            [ring.adapter.jetty :refer [run-jetty]]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.cors :refer [wrap-cors]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults api-defaults]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [ajax.core :as ajax]
            [datascript.core :as d])
  (:gen-class))

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
             :matr.node/source {:db/type :db.type/string
                                :db/cardinality :db.cardinality/one}
             :matr.node/explored {:db/type :db.type/boolean
                                  :db/cardinality :db.cardinality/one}
             :matr.justification/inference-name {:db/type :db.type/string
                                                 :db/cardinality :db.cardinality/one}
             :matr.justification/reiterated-from {:db/type :db.type/ref
                                                  :db/cardinality :db.cardinality/many}
             :matr/kind {:db/type :db.type/keyword
                         :db/cardinality :db.cardinality/one}})
(def db (d/create-conn schema))

(defn make-example-db []
  (let [db (d/create-conn schema)]
    (d/transact! db [{:matr/kind :matr.kind/box
                      :db/id "rootbox"
                      :matr.box/axioms "(AND (a) (b))"
                      :matr.node/_parent
                      [{:db/id "(AND (a) (b))"
                        :matr/kind :matr.kind/node
                        :matr.node/explored false
                        :matr.node/formula "(AND (a) (b))"
                        :matr.node/source "axioms"}]}])
    db))

(defn make-initial-db []
  (let [db (d/create-conn schema)]
    (d/transact! db [{:matr/kind :matr.kind/box}])
    db))

;;; Pure functions over db

(defn db-nodes-query [db boxid formulas]
  (->> (d/q '{:find [?formula ?e]
              :in [$ ?box [?formula ...]]
              :where [[?e :matr.node/parent ?box]
                      [?e :matr.node/formula ?formula]]}
            db
            boxid
            formulas)
       (into {})))

(defn codelet-actions->datoms [db actions]
  (->> (for [action actions]
         (case (get action "actions")
           "addBox" (let [axioms (some-> action (get "axioms") (clojure.string/split #"-"))
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
                       :matr.box/parent (get action "parentBoxid")})
           "addJustification" (let [boxid (get action "boxid")
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
                                 :matr.node/parent boxid})
           "addNode" (if (d/q '[:find ?e . :in $ ?f ?b
                                :where [?e :matr.node/formula ?f] [?e :matr.box/parent ?b]]
                              db (get action "nodeContent") (get action "boxid"))
                       nil
                       {:matr/kind :matr.kind/node
                        :db/id (get action "nodeContent")
                        :matr.node/explored false
                        :matr.node/formula (get action "nodeContent")
                        :matr.node/source (get action "source")
                        :matr.node/parent (get action "boxid")})
           "addAxiom" (let [rootbox (d/q '[:find ?rb . :in $ :where
                                           [?rb :matr/kind :matr.kind/box]
                                           [(missing? $ ?rb :matr.box/parent)]]
                                         db)
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
                           :matr.box/_axioms rootbox}))
           "addGoal" (let [rootbox (d/q '[:find ?rb . :in $ :where
                                          [?rb :matr/kind :matr.kind/box]
                                          [(missing? $ ?rb :matr.box/parent)]]
                                        db)
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
                          :matr.box/_goals rootbox}))))
       (remove nil?)
       (into [])))

(defn codelet-actions->transaction [action-name actions]
  [[:db.fn/call #'matr-core.core/codelet-actions->datoms actions]
   {:db/id :db/current-tx :matr.tx/action-name action-name}])

(defn codelet-response->transactions [resp]
  (let [transactions (->> (get resp "actionDetails")
                          (map first)
                          (map (fn [[action-name actions]]
                                 (codelet-actions->transaction action-name actions))))]
    transactions))

(defn eids->codelet-nodereqs [db eids]
  (->> (d/pull-many db [:matr.node/formula :as :content
                        :matr.node/source :as :source
                        {:matr.node/parent [:db/id]}]
                    eids)
       (map (juxt :matr.node/formula :matr.node/source (comp :db/id :matr.node/parent)))
       (map #(zipmap ["content" "source" "boxid"] %))
       (into [])))

(defn juxt-map [j] (fn [e] (into {} (map (fn [[k f]] [k (f e)]) j))))

(defn db->frontend-json [db]
  (let [conv-links (fn [links kind selector]
                     (fn [justification]
                       (->> justification links (filter #(= kind (:matr/kind %))) (map selector) (into []))))
        conv-node (juxt-map {"content" :matr.node/formula,"explored" :matr.node/explored "checked" (constantly false)})
        conv-justification (juxt-map {"content" :matr.justification/inference-name,
                                      "nextNodes" (conv-links :matr.node/consequents :matr.kind/node :matr.node/formula),
                                      "nextBoxes" (conv-links :matr.node/consequents :matr.kind/box :db/id),
                                      "previousNodes" (conv-links :matr.node/_consequents :matr.kind/node :matr.node/formula),
                                      "prevBoxes" (conv-links :matr.node/_consequents :matr.kind/box :db/id)})
        conv-box (juxt-map {"boxID" :db/id
                            "superBoxID" (comp :db/id :matr.box/parent)
                            "nodes" (fn [b] (->> b :matr.node/_parent
                                                 (filter #(= :matr.kind/node (:matr/kind %)))
                                                 (map conv-node)
                                                 (into [])))
                            "justificationNodes" (fn [b] (->> b :matr.node/_parent
                                                              (filter #(= :matr.kind/justification (:matr/kind %)))
                                                              (map conv-justification)
                                                              (into [])))
                            "axioms" (fn [b] (->> b :matr.box/axioms
                                                  (map #(as-> % f (:matr.node/formula f) {"content" f "checked" true}))
                                                  (into [])))
                            "goals" (fn [b] (->> b :matr.box/goals
                                                 (map #(as-> % f (:matr.node/formula f) {"content" f "checked" false}))
                                                 (into [])))})
        conv-boxes (fn conv-boxes [b] (cons (conv-box b) (->> b :matr.box/_parent (map conv-boxes) flatten)))

        rootbox (d/q '[:find ?rootbox . :where [?rootbox :matr/kind :matr.kind/box] [(missing? $ ?rootbox :matr.box/parent)]] db)
        pull (d/pull db '[:db/id :matr.box/parent
                          {:matr.box/_parent ... 
                           :matr.box/axioms [:matr.node/formula]
                           :matr.box/goals [:matr.node/formula]
                           :matr.node/_parent [:matr/kind :matr.justification/inference-name
                                               :matr.node/formula :matr.node/explored
                                               {:matr.node/consequents [:db/id :matr/kind :matr.node/formula]
                                                :matr.node/_consequents [:db/id :matr/kind :matr.node/formula]}]}] 
                     1)]
    {"box" (into [] (conv-boxes pull))}))

(defn find-justifications-to-reiterate [db nodes]
  (let [justifications (d/q '[:find ?n ?f ?b ?j :in $ % [?n ...] :where
                              [?n :matr.node/formula ?f] [?n :matr.node/parent ?b]
                              (ancestor ?bp ?b)
                              [?j :matr.node/parent ?bp] [?j :matr/kind :matr.kind/justification]
                              (not [?j0 :matr.node/parent ?b] [?j0 :matr.justification/reiterated-from ?j])
                              [?n0 :matr.node/consequents ?j] [?n0 :matr.node/formula ?f]
                              [?n1 :matr.node/consequents ?j] [?n1 :matr.node/source "axioms"]]
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
                                                    :matr.node/source "axioms"
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

;;; Side-effecting stuff

(defn handle-codelet-response [resp]
  (doseq [transaction (codelet-response->transactions resp)]
    (d/transact! db transaction)))

(defn reiterate-justifications [nodes]
  (d/transact! db (find-justifications-to-reiterate @db nodes)))

(defn call-codelets
  ([] (call-codelets ["ALL"] (d/q '[:find [?n ...] :where [?n :matr.node/explored false]] @db)))
  ([nodes] (call-codelets ["ALL"] nodes))
  ([codelets nodes]
   @(ajax/POST "http://localhost:5002/callCodelets"
               {:format :json
                :params {'codeletlist codelets
                         'nodeReq (eids->codelet-nodereqs @db nodes)}
                :handler handle-codelet-response})
   (reiterate-justifications nodes)
   (d/transact! db (->> nodes (map #(vector :db/add % :matr.node/explored true)) (into [])))))

(defn check-formula [formula]
  (try
    (edn/read-string formula)
    true
    (catch java.lang.RuntimeException e
      false)))

(defroutes app-routes
  (GET "/" [] "Hello World")
  (context "/MATRCoreREST/rest/test" req
           (POST "/get/json" {{axioms "axioms", goals "goals", codelets "codelets"} :body}
                 (call-codelets)
                 {:status 200
                  :body (db->frontend-json @db)})
           (POST "/get/checkFormula" req
                 {:status 200
                  :headers {"Content-Type" "applicaton/json"}
                  :body (str (check-formula (:body req)))})
           (POST "/submitActions" {{actions "actions" actionName "actionName"} :body}
                 (d/transact! db (codelet-actions->transaction actionName actions))
                 {:status 200}))
  (route/not-found "Not Found"))

(def app
  (-> #'app-routes
      (wrap-json-body)
      (wrap-json-response)
      (wrap-defaults api-defaults)
      (wrap-cors
       :access-control-allow-origin [#".*"]
       :access-control-allow-methods [:get :post :options])))

(defonce server (atom nil))

(defn start-server! [& []]
  (reset! server
          (run-jetty
           #'app
           {:port 8080
            :join? false})))

(defn stop-server! []
  (when @server
    (.stop @server)
    (reset! server nil)))

(def -main start-server!)

