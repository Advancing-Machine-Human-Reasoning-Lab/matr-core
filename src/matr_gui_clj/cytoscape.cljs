(ns matr-gui-clj.cytoscape
  (:require
   [cljs.reader :as edn]
   [reagent.core :as reagent :refer [atom]]
   [cljsjs.cytoscape]
   [clojure.string :as s]
   [common.latex-converter :refer [s-exp->latex axioms]]))

(defonce cytoscape-instance (atom nil))

(def dagre-layout-options #js{:name "dagre", :nodeSep 150, :fit true, :padding 30})

(def cy-style
  "
node {
  background-color: #b3e5fc;
  shape: ellipse;
  text-valign: center;
  text-halign: center;
}
edge {
  width: 3;
  line-color: #CCC;
  target-arrow-color: #CCC;
  target-arrow-shape: triangle;
  curve-style: bezier;
}
:selected {
  background-color: #007bff;
  color: white;
}
node.main-workspace {
  background-color: white;
  color: rgb(220, 220, 220);
  label: Main Workspace;
  shape: roundrectangle;
  text-valign: bottom;
  text-halign: center;
  border-color: black;
  border-width: 1;
}
node.box {
  background-color: white;
  color: rgb(220, 220, 220);
  label: data(logic);
  shape: roundrectangle;
  text-valign: bottom;
  text-halign: center;
  border-color: black;
  border-width: 1;
}
node.main-workspace:selected {
  background-color: #007bff;
}
node.axiom {
  border-color: gold;
  border-width: 2;
  padding: 3;
}
node.goal {
  border-color: blue;
  border-width: 2;
  padding: 3;
}
node.intermediate-step {
  background-color: gold;
  shape: rectangle;
  label: data(shortString);
  width: label;
}
node.explored {
  background-color: lightpink;
}
node.checked {
  background-color: #b0ff86;
}
node.prove-target {
  background-color: red;
  color: white;
}
node.prove-source {
  background-color: green;
  color: white;
}")

(defn initialize-cytoscape! [element]
  (->> (doto (js/cytoscape #js{:container element
                               :elements #js{:group "nodes"
                                             :data #js{:id "mainWorkspace"
                                                       :shortString "Box-1"}
                                             :classes "main-workspace"}
                               :style cy-style
                               :layout "dagre"
                               :maxZoom 4
                               :minZoom 0.1}))
       (reset! cytoscape-instance)))

(defn layout-cytoscape! []
  (.. @cytoscape-instance
      (layout dagre-layout-options)
      (run)))

(def mapping
  '{FORALL ∀
    FORALL2 ∀
    EXISTS ∃
    EXISTS2 ∃
    PROVES □
    Proves Prvₚₐ
    IMPLIES →
    IFF ⇔
    AND ∧
    OR ∨
    NOT ¬})

(def justification-mapping
  {"ML AND Elimination" "∧-E"
   "ML IMPLIES Elimination" "→-E"
   "ML IMPLIES Introduction" "→-I"})

(defn replace-head-syms [parsed-formula]
  (if (seqable? parsed-formula)
    (let [head (first parsed-formula)]
      (cond
        (get mapping head)
        (cons (get mapping head)
              (map replace-head-syms (rest parsed-formula)))
        (= head 'QB)
        (print-str "⌜" (replace-head-syms (second parsed-formula)) "⌝")
        true (cons head (map replace-head-syms (rest parsed-formula)))))
    parsed-formula))

(defn prettify-node [formula]
  (-> formula
      edn/read-string
      replace-head-syms
      print-str))

;; Added 03/04/20
(defn latex->encoded-svg [^String latex]
  (str "data:image/svg+xml;charset=utf-8,"
       (js/encodeURIComponent
        (str "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" standalone=\"no\"?>"
             (.-innerHTML (.tex2svg js/MathJax latex))))))

(defn str-length->text-width [^String input]
  (do (println "input is " input)
      (reduce + 15 (for [word (s/split input #" |(?=\\)|(?=\()|(?=_)|(?=[{])|(?=\))")]
                     (do
                       (println "word is " word)
                       (if (s/includes? word "\\")
                         12
                         (* (.-length word) 10)))))))

(defn node->cytoscape-nodes [parent {id :db/id, kind :matr/kind, antecedents :matr.node/_consequents, :as node}]
  (cons
   (case kind
     :matr.kind/node
     (let [{formula :matr.node/formula flags :matr.node/flags axiom-of :matr.box/_axioms goal-of :matr.box/_goals} node
           flags (set flags)
           latex-str (s-exp->latex formula)]
       (println formula)
       #js{:group "nodes"
           :style #js{:background-image (latex->encoded-svg latex-str) :width (str-length->text-width latex-str) :height 30 :background-clip "none" :background-fit "none"}
           :data #js{:id (str id) :dbid id :parent (str parent) :matrType "node" :formula formula}
           :classes (str "content-node"
                         (if (contains? flags "checked") " checked")
                         (if (contains? flags "explored") " explored")
                         (if (seq axiom-of) " axiom")
                         (if (seq goal-of) " goal"))})
     :matr.kind/justification
     (let [{name :matr.justification/inference-name} node]
       #js{:group "nodes"
           :data #js{:id (str id) :dbid id :parent (str parent) :shortString (get justification-mapping name name) :matrType "justification"}
           :classes "intermediate-step"})
     (println (str "Got a node with kind: " kind)))
   (for [{srcid :db/id} antecedents]
     #js{:group "edges", :data #js {:id (str srcid "->" id) :source (str srcid) :target (str id)}})))

(defn box->cytoscape-nodes [parent {id :db/id children :matr.box/_parent
                                    nodes :matr.node/_parent logic :matr.box/logic, :as box}]
  (cons
   #js{:group "nodes" :data #js{:id (str id) :shortString ""
                                :matrType "box"
                                :logic logic
                                :dbid id
                                :parent (if parent (str parent) "mainWorkspace")}
       :classes "box"}
   (concat
    (apply concat (map #(node->cytoscape-nodes id %) nodes))
    (apply concat (map #(box->cytoscape-nodes id %) children)))))

(defn cytoscape-load-proof! [proof]
  (let [cy @cytoscape-instance]
    (.remove cy (.$ cy "#1 > *"))
    (.add cy (clj->js (box->cytoscape-nodes nil proof)))))

