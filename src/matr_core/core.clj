(ns matr-core.core
  (:require [clojure.edn :as edn]
            [clojure.core.async :as async]
            [ring.adapter.jetty :refer [run-jetty]]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.cors :refer [wrap-cors]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults api-defaults]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [datascript.core :as d]
            [matr-core.utils :refer [juxt-map]]
            [matr-core.db :refer [schema conn db-rootbox-query]]
            [matr-core.actions :refer [actions->transaction]]
            [matr-core.codelets :refer [step-proofer]])
  (:gen-class))

(defn db->frontend-json
  "Export the database in a format the frontend will understand."
  [db]
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
        ;; Everything but this pull is just for converting the pull
        ;; into the existing format. As an alternative, the frontend
        ;; could be adapted to take this pull directly. The pull could
        ;; even be simplified some, in that case.
        pull (d/pull db '[:db/id :matr.box/parent
                          {:matr.box/_parent ... 
                           :matr.box/axioms [:matr.node/formula]
                           :matr.box/goals [:matr.node/formula]
                           :matr.node/_parent [:matr/kind :matr.justification/inference-name
                                               :matr.node/formula :matr.node/explored
                                               {:matr.node/consequents [:db/id :matr/kind :matr.node/formula]
                                                :matr.node/_consequents [:db/id :matr/kind :matr.node/formula]}]}] 
                     (db-rootbox-query db))]
    {"box" (into [] (conv-boxes pull))}))

(defn check-formula
  "Attempt to parse the formula as edn."
  [formula]
  (try
    (edn/read-string formula)
    true
    (catch java.lang.RuntimeException e
      false)))

(defonce gensym-counter (atom 0))

(defroutes app-routes
  (GET "/" [] "Hello World")
  (context "/MATRCoreREST/rest/test" req
           (POST "/get/json" {{axioms "axioms", goals "goals", codelets "codelets"} :body}
                 (step-proofer)
                 {:status 200
                  :body (db->frontend-json @conn)})
           (POST "/get/checkFormula" req
                 {:status 200
                  :headers {"Content-Type" "applicaton/json"}
                  :body (str (check-formula (:body req)))})
           (POST "/submitActions" {{actions "actions" actionName "actionName"} :body}
                 (d/transact! conn (actions->transaction actionName actions))
                 {:status 200})
           (GET "/query" {{query "query", args "extra-args"} :body}
                {:status 200
                 :body (apply d/q (edn/read-string query) @conn args)})
           (GET "/gensym" []
                {:status 200
                 :body (str (swap! gensym-counter inc))}))
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

(defn make-example-db []
  (let [db (d/create-conn schema)]
    (d/transact! db [{:matr/kind :matr.kind/box}
                     [:db.fn/call #'matr-core.actions/actions->datoms
                      [{"action" "addAxiom"
                        "formula" "(IFF y (NBF (QB y)))"}
                       {"action" "addGoal"
                        "formula" "(AND y (NBF (QB y)))"}]]])
    db))
