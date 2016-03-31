(ns user
  (:require
    [figwheel-sidecar.repl-api :as ra]
    ))

(def figwheel-config
  {:figwheel-options {:server-port 3441
    :load-warninged-code true
    :css-dirs ["resources/public/css"]}
   ;; builds to focus on
   :build-ids        [ "dev" ]
   :load-warninged-code true
   ;; load build configs from project file
   :all-builds [{:id "dev"
     :figwheel true
     :source-paths ["src"]
     :compiler {:main "ifps3.core"
                :asset-path "js/compiled/out"
                :output-to "resources/public/js/compiled/ifps3.js"
                :output-dir "resources/public/js/compiled/out"
                :verbose true}}]
   })


(defn start-dev
  "Start Figwheel and fw repl. You should be running this namespace from PLAIN clojure.main NOT nREPL!
  nREPL support can be had (for server-side code) in parallel, but I've not finished adding it yet (since
  there is no server code yet).
  "
  []
  (ra/start-figwheel! figwheel-config)
  (ra/cljs-repl)
  )

(start-dev)