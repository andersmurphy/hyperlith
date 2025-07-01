(ns app.main
  (:gen-class)
  (:require [hyperlith.core :as h :refer [defview]]))

(def css
  (h/static-css
    [["*, *::before, *::after"
      {:box-sizing :border-box
       :margin     0
       :padding    0}]

     [:html
      {:font-family "Arial, Helvetica, sans-serif"}]

     [:.main
      {:height          :100dvh
       :width           "min(100% - 2rem , 40rem)"
       :margin-inline   :auto
       :padding-block   :2dvh
       :display     :grid
       :place-items :center}]

     [:.counter
      {:text-align :center
       :font-size :50px}]]))

(def shim-headers
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href css}]))

(defview handler-home
  {:path         "/"
   :shim-headers shim-headers
   :on-open
   (fn [{:keys [connected-counter]}]
     (dosync (commute connected-counter inc)))
   :on-close
   (fn [{:keys [connected-counter]}]
     (dosync (commute connected-counter dec)))}
  [{:keys [connected-counter] :as _req}]
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href css}]
    [:main#morph.main
     [:div
      [:p nil "connected users"]
      [:p.counter nil @connected-counter]]]))

(defn ctx-start []
  ;; By using ref and commute to track user count allows for higher
  ;; level of concurrency.
  (let [connected-counter_ (ref 0)]
    (add-watch connected-counter_ :refresh-on-change
      (fn [& _] (h/refresh-all!)))
    {:connected-counter connected-counter_}))

(defn -main [& _]
  (h/start-app
    {:max-refresh-ms 100
     :ctx-start       ctx-start
     :ctx-stop        (fn [_db] nil)
     :csrf-secret    (h/env :csrf-secret)}))

;; Refresh app when you re-eval file
(h/refresh-all!)

(comment
  (-main)
  ;; (clojure.java.browse/browse-url "http://localhost:8080/")

  ;; stop server
  (((h/get-app) :stop))


  ,)
