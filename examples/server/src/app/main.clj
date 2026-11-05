(ns app.main
  (:gen-class)
  (:require
   [hyperlith.core :as h]
   [billion-cells.main :as cells]
   [billion-checkboxes.main :as boxes]
   [game-of-life.main :as gol]))

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

(comment
  @app_
  
  )
