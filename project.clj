(defproject ifps3 "0.1.0-SNAPSHOT"
  
  :dependencies [[org.clojure/clojure "1.7.0" :scope "provided"]
                 [org.clojure/clojurescript "1.7.170" :scope "provided"]
                 [com.cognitect/transit-clj "0.8.281" :scope "provided"]
                 [com.cognitect/transit-cljs "0.8.225" :scope "provided"]
                 [figwheel-sidecar "0.5.0-2" :scope "test"]
                 [org.omcljs/om "1.0.0-alpha26"]
                 [org.clojure/core.async "0.2.371"]
                 [selfsame/dollar "0.0.9-SNAPSHOT"]
                 [selfsame/pdfn "1.0.1-SNAPSHOT"]]

  :plugins [[lein-cljsbuild "1.1.1"]]

  :source-paths ["src"]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]

  :cljsbuild {
    :builds [{:id "dev"
              :source-paths ["src"]
              :figwheel true
              :compiler {:main ifps3.core
                         :asset-path "js/compiled/out"
                         :output-to "resources/public/js/compiled/ifps3.js"
                         :output-dir "resources/public/js/compiled/out"
                         :source-map-timestamp true }}
             {:id "min"
              :source-paths ["src"]
              :compiler {:output-to "resources/public/js/compiled/ifps3.js"
                         :main ifps3.core
                         :optimizations :advanced
                         :pretty-print false}}]}

  :figwheel {
    :server-port 3441 
    :css-dirs ["resources/public/css"] })
 