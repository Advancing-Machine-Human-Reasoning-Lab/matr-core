(ns matr-core.core-test
  (:require [clojure.test :refer :all]
            [matr-core.core :as mcc]
            [common.latex-converter :as clc]))

(deftest axiom-test
  (testing "Axioms are correctly converted to latex"
    (is (= (map clc/s-exp->latex clc/axioms)
           '("\\exists_{\\psi} \\square (\\text{PA}, (\\psi \\leftrightarrow \\neg (\\text{Prv}_{\\text{PA}} (\\ulcorner {\\psi} \\urcorner))))"
             "\\forall_{\\psi} \\square (\\text{PA}, (\\text{Opposite} (\\ulcorner {\\psi} \\urcorner, \\ulcorner {\\neg \\psi} \\urcorner))) \\wedge (\\text{Opposite} (\\ulcorner {\\psi} \\urcorner, \\ulcorner {\\neg \\psi} \\urcorner))"
             "\\forall_{\\phi} \\square (\\text{PA}, \\phi) \\rightarrow (\\text{Prv}_{\\text{PA}} (\\ulcorner {\\phi} \\urcorner))"
             "\\forall_{\\phi} \\square (\\text{PA}, (\\text{Prv}_{\\text{PA}} (\\ulcorner {\\phi} \\urcorner))) \\rightarrow (\\text{Prv}_{\\text{PA}} (\\ulcorner {\\phi} \\urcorner))"
             "\\forall_{\\text{n}} \\forall_{\\text{m}} ((\\text{Opposite} (\\text{n}, \\text{m}) \\wedge (\\text{Prv}_{\\text{PA}} (\\text{n}))) \\rightarrow \\neg (\\text{Prv}_{\\text{PA}} (\\text{m})))"
             "\\forall_{\\phi} \\forall_{\\psi} \\square (\\text{PA}, (\\text{Prv}_{\\text{PA}} (\\ulcorner {\\phi \\rightarrow \\psi} \\urcorner) \\rightarrow (\\text{Prv}_{\\text{PA}} (\\ulcorner {\\phi} \\urcorner) \\rightarrow (\\text{Prv}_{\\text{PA}} (\\ulcorner {\\psi} \\urcorner)))))"
             "\\forall_{\\phi} \\square (\\text{PA}, (\\text{Prv}_{\\text{PA}} (\\ulcorner {\\phi} \\urcorner) \\rightarrow (\\text{Prv}_{\\text{PA}} (\\ulcorner {\\text{Prv}_{\\text{PA}} (\\ulcorner {\\phi} \\urcorner)} \\urcorner))))"
             "\\neg (\\exists_{\\phi} \\square (\\text{PA}, \\phi) \\wedge \\square (\\text{PA}, \\neg \\phi))"
             "\\exists_{\\phi} \\neg \\square (\\text{PA}, \\phi) \\wedge \\neg \\square (\\text{PA}, \\neg \\phi)"
             "\\square (\\text{PA}, \\forall_{\\text{n}} \\forall_{\\text{m}} ((\\text{Opposite} (\\text{n}, \\text{m}) \\wedge (\\text{Prv}_{\\text{PA}} (\\text{n}))) \\rightarrow \\neg (\\text{Prv}_{\\text{PA}} (\\text{m})))) \\rightarrow \\neg \\neg (\\exists_{\\text{p}} \\square (\\text{PA}, \\text{p}) \\wedge \\square (\\text{PA}, \\neg \\text{p}))")))))

(deftest start+stop-backend
  (testing "Backend server properly starts"
    (let [server-return (mcc/start-server!)]
      (are [test-type actual-type] (instance? test-type actual-type)
        org.eclipse.jetty.server.Server                    (server-return :server)
        clojure.core.async.impl.channels.ManyToManyChannel (server-return :cin))))
  (testing "Backend server properly stops"
    (is (mcc/stop-server!))))
