(ns matr-gui-clj.state
  (:use [reagent.core :only [atom]]))

(defonce app-state (atom {:modal-show false
                          :modal-title nil
                          :modal-selection ""
                          :modal-data {:formula nil
                                       :formula-valid? false}
                          :error {:show-error? false
                                  :title nil
                                  :message nil}
                          :cursor-style "auto"
                          :create-node-date {}
                          :onclick-event-debug nil
                          :restore-minimal-proof false}))


(defonce cy-state (atom {:elements {:nodes []
                                    :edges []}}))


(defonce action-queue (atom []))

