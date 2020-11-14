(ns matr-gui-clj.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as reagent :refer [atom]]
            [cljsjs.jquery]
            [clojure.string :as str]
            [cljsjs.cytoscape :as cy]
            [cljs.js]
            [antizer.reagent :as ant]
                                        ;            [datafrisk.core :as d] 
            [matr-gui-clj.net :as net]
            [matr-gui-clj.utils :as utils]
            [matr-gui-clj.cytoscape :refer [cytoscape-instance initialize-cytoscape! layout-cytoscape! cytoscape-load-proof!]]
            [cljs.core.async :as async :refer [>! <! chan buffer close!
                                               alts! take! put! timeout]])
  (:use [jayq.core :only [$ css html]]
        [matr-gui-clj.state :only [app-state
                                   cy-state]]))
;; define your app data so that it doesn't get over-written on reload

(defn reload-proof! []
  (go
    (cytoscape-load-proof! (:body (<! (net/get-proof))))
    (layout-cytoscape!)))

(defn add-node-form [display-buttons?]
  (fn []
    (let [form (ant/get-form)
          form-style {:label-col {:span 3}
                      :wrapper-col {:span 18}}]
      [ant/form {:layout "horizontal"}
       [ant/form-item (merge form-style {:label "Formula"})
        (ant/decorate-field form "formula"
                            [ant/input {:on-change (fn [e v d]
                                                     (let [result (. js/parinfer indentMode e.target.value #js{:cursorLine 0
                                                                                                               :cursorX e.target.selectionStart})]
                                                       (set! e.target.value (.-text result))
                                                       (.setSelectionRange e.target result.cursorX result.cursorX)))}])]
       (if display-buttons?
         [ant/form-item {:wrapper-col {:offset 6}
                         :style {:margin-left "50%"
                                 :margin-bottom "0px"}}
          [ant/row {:span "100%"}
           [ant/col {:span 4}
            [ant/button {:type "primary"
                         :on-click (fn []
                                     (ant/validate-fields form)
                                     (let [formula (ant/get-field-value form "formula")]
                                       (go 
                                         (if (<! (net/check-formula formula))
                                           (do
                                             (<! ((-> @app-state :node-submit-func) formula))
                                             (reload-proof!)
                                             (swap! app-state assoc :modal-show false))
                                           (ant/notification-open
                                            {:message "Invalid formula!"
                                             :duration 3})))))}
             "Submit"]]
           [ant/col {:span 4
                     :style {:margin-left "50px"}}
            [ant/button {
                         :on-click (fn[]
                                     (ant/reset-fields form)
                                     (swap! app-state assoc :modal-show false))}
             "Cancel"]]]])])))

(defn upload-config-form []
  (let [file (atom nil)]
    (fn []
      (let [form (ant/get-form)
            form-style {:label-col {:span 3}
                        :wrapper-col {:span 18}}]
        [ant/form {:layout "horizontal"}
         [ant/form-item form-style
          (ant/decorate-field form "reset" [ant/checkbox "Reset DB?"])]
         [ant/form-item
          (ant/decorate-field form "config"
                              [ant/upload {:name "config"
                                           :beforeUpload (fn [file-obj]
                                                           (reset! file file-obj)
                                                           false)}
                               [ant/button [ant/icon {:type "upload"}] "Click to upload"]])]
         [ant/form-item {:wrapper-col {:offset 6}
                         :style {:margin-left "50%"
                                 :margin-bottom "0px"}}
          [ant/row {:span "100%"}
           [ant/col {:span 4}
            [ant/button {:type "primary"
                         :on-click (fn [] (ant/validate-fields form)
                                     (go (<! (net/upload-config! @file (ant/get-field-value form "reset")))
                                         (reload-proof!)
                                         (swap! app-state assoc :modal-show false)))}
             "Submit"]]
           [ant/col {:span 4
                     :style {:margin-left "50px"}}
            [ant/button {
                         :on-click (fn[]
                                     (ant/reset-fields form)
                                     (swap! app-state assoc :modal-show false))}
             "Cancel"]]]]]))))

(defn select-modal-form [selection]
  (case selection
    "add-node" (add-node-form true)
    "upload-config" (upload-config-form)
    [:div]))

(defn error-message []
  (if (= (:error :show-error? @app-state) true)
    (do
      (ant/notification-open
       {:message (:error :title @app-state)
        :description (:error :title @app-state)
        :duration 3})
      (swap! app-state assoc-in [:error :show-error?] false))))

(defn modal []
  (let []
    (fn []
      [ant/modal {:visible (get @app-state :modal-show)
                  :title (get @app-state :modal-title)
                  :footer nil
                  :on-ok (fn []
                           (swap! app-state assoc :modal-show false)
                           )
                  :on-cancel #(swap! app-state assoc :modal-show false)}
       [ant/create-form (select-modal-form (get @app-state :modal-selection))]])))


(defn side-nav-bar-btn
  ([label btn-class]
  [:li {:class "list-item"}
   [ant/button {:class (clojure.string/join " " ["nav-pill" "nav-link" btn-class])
                :type "ghost"
                :style {:width "100%"}
                :on-click (fn[e]
                    (utils/select ($ (str "." btn-class)))
                    (comment "Open Modal"))} label]])
  ([label btn-class clickfunc]
   [:li {:class "list-item"}
    [ant/button {:class (clojure.string/join " " ["nav-pill" "nav-link" btn-class])
                 :type "ghost";
                 :style {:width "100%"}
                 :on-click (fn [e]
                             (utils/select ($ (str "." btn-class)))
                             (comment "Open Modal")
                             (clickfunc e app-state))} label]]))

(defn side-nav-bar
  []
  [:nav {:id "side-nav"
         :class "flex-column"}
   [:ul {:class "nav-pills"
         :style {:list-style-type "none"
                 :padding "0px"}}
    (side-nav-bar-btn (if (:restore-minimal-proof @app-state)
                        "Switch to full proof"
                        "Switch to min proof")
                      "btn-min-proof"
                      (fn []
                        (swap! app-state update :restore-minimal-proof not)
                        (reload-proof!)))
    (side-nav-bar-btn "Add Axiom" "btn-add-axiom" utils/add-axiom)
    (side-nav-bar-btn "Add Node" "btn-add-node" utils/add-node)
    (side-nav-bar-btn "Add Goal" "btn-add-goal" utils/add-goal)
    (side-nav-bar-btn "Sync Proof" "btn-sync-proofer" reload-proof!)
    (side-nav-bar-btn "Step Proof" "btn-step-proofer"
                      (fn [] (go (cytoscape-load-proof! (:body (<! (net/step-proof!))))
                                 (layout-cytoscape!))))
    (side-nav-bar-btn "Upload config" "btn-upload-config"
                      (fn []
                        (swap! app-state merge
                               {:modal-show true
                                :modal-title "Upload Config"
                                :modal-selection "upload-config"})))
    (side-nav-bar-btn "Interference Rules" "btn-inference-rules")
    (side-nav-bar-btn "Get Node's Proof" "btn-add-node"
                      utils/select-node-for-proof)
    (side-nav-bar-btn "Export Proof to PNG" "btn-export-node" utils/export-proof-to-png)
   [:li {:class "list-item"}
    [ant/button {:class (clojure.string/join " " ["nav-pill" "nav-link"])
                 :type "ghost";
                 :style {:width "100%"}
                 :href (str net/server "generateProofJSON")}
                      "Proof json with latex"]]]])

(defn cs []
  (let [el (atom nil)]
    (reagent/create-class
     {:display-name "cytoscape"
      :component-did-mount (fn [] (initialize-cytoscape! @el))
      :reagent-render (fn [] [:div {:id "cytoscape-renderer"
                                    :ref (fn [e] (reset! el e))
                                    :style {:width "100%"
                                            :height "100%"}}])})))

(defn main-content []
  [:div {:id "main-content"
         :class "mdl-grid main-content"
         :style {:cursor (get @app-state :cursor-style)}}
   [:div {:id "cy-container"
           :class "card card-body"
           :style {:position "fixed"}}
     [:img {:src "/images/amhrlogo.png"
            :alt "AMHR logo"
            :class "amhrlogo-overlay"}]
     [:div {:id "cy"}
      [cs]]]])

(defn render-cy-els []
  (js/add cs @cy-state))

(reagent/render-component [:div
                           [side-nav-bar]
                           [main-content]
                           [modal]]
                           ;[d/DataFriskShell (get @app-state :onclick-event-debug)]
                          (. js/document (getElementById "main-content-container")))
(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc))
)
