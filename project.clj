(defproject matr-core "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/core.async "0.4.490"]
                 [ring-cors "0.1.13"]
                 [metosin/compojure-api "2.0.0-alpha30"]
                 [ring/ring-jetty-adapter "1.7.1"]
                 [cljs-ajax "0.8.0"]
                 [datascript "0.18.2"]
                 [com.taoensso/timbre "4.10.0"]]
  :main ^:skip-aot matr-core.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
