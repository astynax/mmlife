(defproject mmlife "0.1.0-SNAPSHOT"
  :description "MMLife Game Server"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2202"]
                 [prismatic/dommy "0.1.2"]
                 [compojure "1.1.8"]
                 [http-kit "2.1.18"]
                 [ring "1.2.1"]
                 [sonian/carica "1.1.0" :exclusions [[cheshire]]]]
  :plugins [[lein-cljsbuild "1.0.3"]]
  :main mmlife.core
  :aot [mmlife.core]
  :cljsbuild {:builds {:dev
                       {:source-paths ["src-cljs"]
                        :jar true
                        :compiler {:output-to "resources/public/js/main.js"
                                   :optimizations :whitespace
                                   :pretty-print true}}
                       :prod
                       {:source-paths ["src-cljs"]
                        :compiler {:output-to "resources/public/js/main.js"
                                   :optimizations :advanced
                                   :pretty-print false}}}})
