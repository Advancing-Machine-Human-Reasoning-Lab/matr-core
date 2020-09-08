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
            [matr-core.utils :refer [juxt-map db-restricted]]
            [matr-core.db :as db :refer [schema conn db-rootbox-query extract-proof-eids transact!]]
            [matr-core.actions :refer [actions->transaction Action]]
            [matr-core.codelets :refer [step-proofer]])
  (:gen-class))

(defn db->simple-frontend-json
  [db minimal]
  (let [db (if minimal (db-restricted db (extract-proof-eids db)) db)]
    (d/pull db '[:db/id :matr.box/parent
                 {(:limit :matr.box/_parent nil) ...
                  (:limit :matr.box/axioms nil) [:matr.node/formula]
                  (:limit :matr.box/goals nil) [:matr.node/formula]
                  (:limit :matr.node/_parent nil)
                  [:db/id :matr/kind :matr.justification/inference-name
                   :matr.node/formula (:limit :matr.node/flags nil)
                   :matr.box/_axioms :matr.box/_goals
                   {:matr.node/consequents [:db/id :matr/kind :matr.node/formula]
                    :matr.node/_consequents [:db/id :matr/kind :matr.node/formula]}]}]
            (db-rootbox-query db))))

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
  (transact! conn [{:matr/kind :matr.kind/codelet
                    :matr.codelet/endpoint endpoint
                    :matr.codelet/query query
                    :matr.codelet/stage stage
                    :matr.codelet/query-include-since since
                    :matr.codelet/transaction-since d/tx0}]))

(defn process-config [{:keys [root_logic codelets actions]}]
  (alter-var-root #'conn (constantly (db/make-initial-db root_logic)))
  (doseq [{:keys [query endpoint stage since] :or {stage 0 since false}} codelets]
    (register-codelet! query endpoint stage since))
  (transact! conn (actions->transaction actions)))

(def app
  (-> 
   (api
    (GET "/proof" []
      :query-params [minimalResponse :- schema/Bool]
      (resp/ok (db->simple-frontend-json @conn minimalResponse)))
    (GET "/nodeProof" []
      :query-params [nodeId :- schema/Int]
      (let [json (db->simple-frontend-json
                      (db-restricted
                       @conn
                       (conj
                        (db/find-proof-of @conn
                                          (db/compute-distances @conn) nodeId)
                        (db-rootbox-query @conn))) false)]
            (resp/ok json)))
    (POST "/stepProofer" []
      :query-params [minimalResponse :- schema/Bool]
      (let [c (async/chan)]
        (async/>!! (:cin @server) {:type :step :reply-chan c})
        (async/<!! c))
      (resp/ok (db->simple-frontend-json @conn minimalResponse)))
    (context "/MATRCoreREST/rest/test" []
      (POST "/config" []
        :multipart-params [f :- upload/ByteArrayUpload]
        :middleware [#(upload/wrap-multipart-params % {:store (byte-array-store)})]
        (process-config (spy (yaml/parse-string (slurp (:bytes f)) :keywords true)))
        (resp/ok "Foo!"))
      (POST "/get/checkFormula" req
        (resp/ok (str (check-formula (spy (slurp (:body req)))))))
      (POST "/submitActions" []
        :body [{:keys [actions actionName]} ActionRequest]
        (clojure.pprint/pprint actions)
        (transact! conn (actions->transaction actionName actions))
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
    (transact! db [{:matr/kind :matr.kind/box}
                   [:db.fn/call #'matr-core.actions/actions->datoms
                    [{"action" "addAxiom"
                      "formula" "(IFF y (NBF (QB y)))"}
                     {"action" "addGoal"
                      "formula" "(AND y (NBF (QB y)))"}]]])
    db))
