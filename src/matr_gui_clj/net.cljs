(ns matr-gui-clj.net
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs-http.client :as http]
            [cljs.core.async :as async :refer [>! <! chan buffer close!
                                               alts! take! put! timeout]])
  (:use [matr-gui-clj.state :only [app-state
                                   cy-state
                                   action-queue]]))

(def server "http://localhost:8080/")

(defn change-server [server-path]
  (set! server server-path))

(def response-data (atom {:body nil}))

(defn check-formula [formula]
  (go (let [response (<! (http/post (str server "MATRCoreREST/rest/test/get/checkFormula")
                                    {:with-credentials? false
                                     :edn-params {:body (str formula)}}))]
        (= (:body response) "true"))))

(defn send-actions!
  ([] (go (let [response (<! (http/post (str server "MATRCoreREST/rest/test/submitActions")
                                        {:with-credentials? false
                                         :json-params {:actions @action-queue}}))]
            (println response))))
  ([action] (go (let [response (<! (http/post (str server "MATRCoreREST/rest/test/submitActions")
                                              {:with-credentials? false
                                               :json-params {:actions [action]
                                                             :actionName (:action action)}}))]
                  (println response)))))

(defn get-query []
  (comment "Implement Query"))

(defn post-query []
  (comment "Implement Query"))

(defn get-proof [] (http/get (str server "proof")
                             {:with-credentials? false :accept "application/edn"
                              :query-params {:minimalResponse (:restore-minimal-proof @app-state)}}))
(defn get-node-proof [^Long node]
  (http/get (str server "nodeProof")
            {:with-credentials? false :accept "application/edn"
             :query-params {:nodeId node}}))

(defn step-proof! [] (http/post (str server "stepProofer")
                                {:with-credentials? false :accept "application/edn"
                                 :query-params {:minimalResponse (:restore-minimal-proof @app-state)}}))

(defn send-axiom! [formula] (send-actions! {:action "addAxiom" :formula formula}))
(defn send-node! [box formula] (send-actions! {:action "addNode" :formula formula :box box}))
(defn send-goal! [formula] (send-actions! {:action "addGoal" :formula formula}))

(defn upload-config! [file reset?]
  (http/post (str server "MATRCoreREST/rest/test/config")
             {:with-credentials? false
              :query-params {:reset reset?}
              :multipart-params [["f" file]]}))
