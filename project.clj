(defproject matr-core "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/core.async "0.4.490"]
                 [ring/ring "1.7.1"]
                 [ring/ring-defaults "0.3.2"]
                 [ring/ring-json "0.4.0"]
                 [compojure "1.6.1"]
                 [ring-cors "0.1.13"]
                 [cljs-ajax "0.8.0"]
                 [datascript "0.18.2"]]
  :main ^:skip-aot matr-core.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
