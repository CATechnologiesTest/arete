(defproject engine "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main ^:skip-aot engine.core
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.flatland/ordered "1.5.6"]
                 [clj-yaml "0.4.0"]
                 [potemkin "0.4.5"]
                 [org.clojure/data.json "0.2.6"]
                 [org.javasimon/javasimon-core "4.1.3"]]
  :profiles {:uberjar {:aot :all}})

