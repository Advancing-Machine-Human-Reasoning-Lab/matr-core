(ns matr-gui-clj.utils
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljsjs.jquery]
            [clojure.string :as str]
            [cljsjs.cytoscape]
            [cljs.js]
            [datafrisk.core :as d]
            [matr-gui-clj.net :as net]
            [antizer.reagent :as ant]
            [cljsjs.filesaverjs :as fsjs])
  (:use [matr-gui-clj.state :only [app-state
                                   cy-state
                                   action-queue]]
        [matr-gui-clj.cytoscape :only [cytoscape-instance]]
        [jayq.core :only [$ css html]]))

(defn change-cursor [newcursor]
  (swap! app-state assoc :cursor-style newcursor))

(defn create-node []
  (change-cursor "crosshair"))

(defn error [title message]
  (swap! app-state assoc-in [:error :show-error?] true)
  (swap! app-state assoc-in [:error :title] title)
  (swap! app-state assoc-in [:error :message] message))

(defn axiom-submit
  "Submit the user's axiom formula if it is parsed as a valid-formula,
   otherwise inform the user it is invalid"
  [formula]
  (let [valid-formula? (net/check-formula formula)]
    (swap! app-state assoc-in [:modal-data :formula-valid?] valid-formula?)
    (if valid-formula?
      true
      (do (error "Invalid Formula!" "The MATR Software was unable to parse the provided expression!")
          (axiom-submit formula)))))

(defn export-proof-to-png []
  (let [cy @cytoscape-instance
        image-data (.png cy #js{:output "blob"
                                :full true})
        name "Proof_Out.png"]
    (js/saveAs image-data name)))

(defn select [selector]
  (.removeClass ($ ".ant-btn-primary") "ant-btn-primary")
  (.addClass selector "ant-btn-primary"))

(defn add-node [e app-state]
  (change-cursor "crosshair")
  (.one @cytoscape-instance "tap"
        (fn [click-event]
          (change-cursor "auto")
          (let [target-node (.-target click-event)
                boxid (case (.data target-node "matrType")
                        ("node" "justification") (.. target-node (parent) (data "dbid"))
                        "box" (.data target-node "dbid")
                        nil)]
            (when boxid
              (swap! app-state assoc :modal-show true)
              (swap! app-state assoc :modal-title "Node")
              (swap! app-state assoc :modal-selection "add-node")
              (swap! app-state assoc :node-submit-func (partial net/send-node! boxid)))))))

(defn select-node-for-proof
  "Allow the user to select an aribtrary node from the graph of the proof and
   then proceed to pull the (minimal) graph of the proof of that particular
   node."
  []
  (change-cursor "crosshair")
  (.one @cytoscape-instance "tap"
        (fn [click-event]
          (change-cursor "auto")
          (let [target-node (.-target click-event)
                boxid (case (.data target-node "matrType")
                        ("node" "justification") (.. target-node (parent) (data "dbid"))
                        "box" (.data target-node "dbid")
                        nil)]
            (when boxid
              ;; Wait for channel to return graph of the proof of target node
              ;; (the node the user selected) and then update front-end to
              ;; display this
              (go
                (matr-gui-clj.cytoscape/cytoscape-load-proof!
                 (:body (<! (net/get-node-proof
                             (.data target-node "dbid")))))
                (matr-gui-clj.cytoscape/layout-cytoscape!)))))))

(defn add-axiom [e app-state]
  (swap! app-state assoc :modal-show true)
  (swap! app-state assoc :modal-title "Add Axiom")
  (swap! app-state assoc :modal-selection "add-node")
  (swap! app-state assoc :node-submit-func net/send-axiom!))

(defn add-goal [e app-state]
  (swap! app-state assoc :modal-show true)
  (swap! app-state assoc :modal-title "Add Goal")
  (swap! app-state assoc :modal-selection "add-node")
  (swap! app-state assoc :node-submit-func net/send-goal!))

