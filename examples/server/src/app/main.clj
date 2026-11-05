(ns app.main
  (:gen-class)
  (:require
   [hyperlith.core :as h]
   [billion-cells.main :as cells]
   [billion-checkboxes.main :as boxes]
   [game-of-life.main :as gol]
   [clj-async-profiler.core :as prof]))

(defonce app_ (atom nil))

(defn start-app! [& {:keys [mode]}]
  (reset! app_
    (h/start-server!
      {:apps               [cells/app boxes/app gol/app]
       :tick-ms            100
       :render-buffer-size (* 64 16384)
       :email              (h/env :email)
       :mode               mode})))

(defn -main [& _]
  (start-app! :mode :prod))

(comment
  (do (start-app! :mode :dev) nil)
  
  ;; (clojure.java.browse/browse-url "http://example.andersmurphy.com.localhost:8080/")
  
  ;; (clojure.java.browse/browse-url "http://cells.andersmurphy.com.localhost:8080/")
  
  ;; (clojure.java.browse/browse-url "http://checkboxes.andersmurphy.com.localhost:8080/")

  ,)

(comment ;; Profiling

  (prof/start {:event :alloc})
  (prof/start)
  (prof/stop)
  (prof/serve-ui 7777)
  ;; (clojure.java.browse/browse-url "http://localhost:7777/")
  )

(comment ;; stubbed load testing

  (def stub-router
    (do (start-app! :mode :test)
        (@app_ ::h/wrapped-router)))

  (count (@app_ ::h/conns))

  (defn edn->json-stream [edn]
    (java.io.ByteArrayInputStream.
      (String/.getBytes (h/edn->json edn) "UTF-8")))

  (dotimes [i 5000]
    (stub-router
      {:request-method :post
       :uri            "/"
       :headers        {"host"            "checkboxes.andersmurphy.com"
                        "content-type"    "application/json"
                        "accept-encoding" "zstd"
                        "sec-fetch-site"  "same-origin"
                        "cookie"          (str "__Host-sid=" "test-user-" i)}
       :body           (edn->json-stream {"tabid" "7dc673ca"})}))

  (do
    (dotimes [_ 100000]
      (stub-router
        {:host           "checkboxes.andersmurphy.com"
         :request-method :post
         :uri            "/t_rqnpSL_NvK8EJhoBwkc6TNJ4VsLi1Fs"
         :headers        {"host"            "checkboxes.andersmurphy.com"
                          "content-type"    "application/json"
                          "accept-encoding" "zstd"
                          "sec-fetch-site"  "same-origin"
                          "cookie"          "__Host-sid=test-user-1"}
         :body           (edn->json-stream
                           {"tabid"    "7dc673ca"
                            "targetid" (str (rand-int 255))
                            "parentid" (str (rand-int 1000))})}))
    (println "Done"))

  (def tx! (get-in @app_ [::h/db-txs! "checkboxes.andersmurphy.com"]))

  ;; simulate slow TX
  (do (dotimes [_ 300]
        (tx! (fn [_ _] (Thread/sleep 90)))
        (Thread/sleep 101))
      (println "Done")))
