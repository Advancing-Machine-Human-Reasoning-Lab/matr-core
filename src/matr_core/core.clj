(ns matr-core.core
  (:require [clojure.edn :as edn]
            [clojure.core.async :as async]
            [ring.adapter.jetty :refer [run-jetty]]
            [compojure.api.sweet :refer :all]
            [compojure.route :as route]
            [ring.middleware.cors :refer [wrap-cors]]
            [ring.middleware.multipart-params.byte-array :refer [byte-array-store]]
            [compojure.api.upload :as upload]
            [schema.core :as schema]
            [ring.util.http-response :as resp]
            [datascript.core :as d]
            [taoensso.timbre :as timbre :refer [log spy]]
            [yaml.core :as yaml]
            [matr-core.utils :refer [juxt-map]]
            [matr-core.db :refer [schema conn db-rootbox-query]]
            [matr-core.actions :refer [actions->transaction Action]]
            [matr-core.codelets :refer [step-proofer]])
  (:gen-class))

(defn db->frontend-json
  "Export the database in a format the frontend will understand."
  [db]
  (let [conv-links (fn [links kind selector]
                     (fn [justification]
                       (->> justification links (filter #(= kind (:matr/kind %))) (map selector) (into []))))
        check-explored-flag (fn [n] (some #(= % "explored") (:matr.node/flags n)))
        check-checked-flag (fn [n] (some #(= % "checked") (:matr.node/flags n)))
        conv-node (juxt-map {"content" :matr.node/formula,"explored" check-explored-flag "checked" check-checked-flag})
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
                                                 (map #(as-> % f (:matr.node/formula f) {"content" f}))
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
                                               :matr.node/formula :matr.node/flags
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

(defonce server (atom nil))

(schema/defschema RegisterCodeletRequest
  {:query schema/Str
   :endpoint schema/Str
   (schema/optional-key :stage) schema/Int
   (schema/optional-key :since) schema/Bool})

(schema/defschema ActionRequest
  {:actions [Action]
   :actionName schema/Str})

(schema/defschema QueryRequest
  {:query schema/Str
   :extra-args [schema/Any]})

(defn register-codelet! [query endpoint stage since]
  (d/transact! conn [{:matr/kind :matr.kind/codelet
                      :matr.codelet/endpoint endpoint
                      :matr.codelet/query query
                      :matr.codelet/stage stage
                      :matr.codelet/query-include-since since
                      :matr.codelet/transaction-since d/tx0}]))

(defn process-config [{:keys [codelets actions]}]
  (doseq [{:keys [query endpoint stage since] :or {stage 0 since false}} codelets]
    (register-codelet! query endpoint stage since))
  (d/transact! conn (actions->transaction actions)))

(def app
  (-> 
   (api
    (context "/MATRCoreREST/rest/test" []
      (POST "/config" []
        :multipart-params [f :- upload/ByteArrayUpload]
        :middleware [#(upload/wrap-multipart-params % {:store (byte-array-store)})]
        (process-config (spy (yaml/parse-string (slurp (:bytes f)) :keywords true)))
        (resp/ok "Foo!"))
      (POST "/get/json" []
        (let [c (async/chan)]
          (async/>!! (:cin @server) {:type :step :reply-chan c})
          (async/<!! c))
        (resp/ok (db->frontend-json @conn)))
      (POST "/stepProofer" []
        (let [c (async/chan)]
          (async/>!! (:cin @server) {:type :step :reply-chan c})
          (async/<!! c))
        (resp/ok (db->frontend-json @conn)))
      (POST "/get/checkFormula" req
        (resp/ok (str (check-formula (slurp (:body req))))))
      (POST "/submitActions" []
        :body [{:keys [actions actionName]} ActionRequest]
        (clojure.pprint/pprint actions)
        (d/transact! conn (actions->transaction actionName actions))
        (resp/ok))
      (GET "/query" []
        :body [{query :query, args :extra-args} QueryRequest]
        (let [res (apply d/q (edn/read-string query) @conn args)]
          (resp/ok (if (coll? res) res (str res)))))
      (POST "/query" []
        :body [{query :query, args :extra-args} QueryRequest]
        (let [res (apply d/q (edn/read-string query) @conn args)]
          (resp/ok (if (coll? res) res (str res)))))
      (GET "/gensym" []
        :return schema/Int
        :summary "Return a fresh integer for use in symbol generation"
        (resp/ok (swap! gensym-counter inc)))
      (POST "/registerCodelet" []
        :body [{:keys [query endpoint stage since] :or {stage 0 since false}} RegisterCodeletRequest]
        :summary "Register a codelet"
        (register-codelet! query endpoint stage since)
        (resp/ok))
      (undocumented
       (route/not-found {:not "found"}))))
   (wrap-cors
    :access-control-allow-origin [#".*"]
    :access-control-allow-methods [:get :post :options])))

(defn start-server! [& []]
  (let [inp (async/chan)]
    (async/go-loop []
      (let [message (async/<! inp)]
        (case (:type message)
          :step (do
                  (try
                    (step-proofer)
                    (async/>! (:reply-chan message) :done)
                    (catch Exception e
                      (println "Whelp, that didn't go well.")
                      (println (.getMessage e))
                      (.printStackTrace e)
                      (async/>! (:reply-chan message) :error!)))
                  (recur))
          :exit (when-let [s @server]
                  (.stop (:server s))
                  (reset! server nil)))))
    (reset! server
            {:server (run-jetty
                      #'app
                      {:port 8080
                       :join? false})
             :cin inp})))

(defn stop-server! []
  (when-let [s @server]
    (async/>!! (:cin s) {:type :exit})))

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
