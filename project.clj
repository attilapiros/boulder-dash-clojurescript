(defproject boulderdash "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [ring "1.3.0"]
                 [org.clojure/clojurescript "0.0-2280"
                  :exclusions [org.apache.ant/ant]]]
  :plugins [[lein-cljsbuild "1.0.3"]]
  :hooks [leiningen.cljsbuild]
  :source-paths ["src/clj"]
  :cljsbuild {
    :builds {
      :main {
        :source-paths ["src/cljs"]
        :compiler {:output-to "resources/public/js/cljs.js"
                   :optimizations :simple
                   :pretty-print true}
        :jar true}}}
  :main my-boulder-dash.server)

