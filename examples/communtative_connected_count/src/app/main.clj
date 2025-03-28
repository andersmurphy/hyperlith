(ns app.main
  (:gen-class)
  (:require [hyperlith.core :as h]))

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

(defn render-home [{:keys [connected-counter] :as _req}]
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}]
    [:main#morph.main
     [:div
      [:p nil (str "connected users")]
      [:p.counter nil @connected-counter]]]))

(def default-shim-handler
  (h/shim-handler
    (h/html
      [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}])))

(def router
  (h/router
    {[:get (css :path)] (css :handler)
     [:get  "/"]        default-shim-handler
     [:post "/"]        (h/render-handler #'render-home
                          :on-open
                          (fn [{:keys [connected-counter]}]
                            (dosync (commute connected-counter inc)))
                          :on-close
                          (fn [{:keys [connected-counter]}]
                            (dosync (commute connected-counter dec))))}))

(defn ctx-start []
  ;; By using ref and commute to track user count allows for higher
  ;; level of concurrency.
  (let [connected-counter_ (ref 0)]
    (add-watch connected-counter_ :refresh-on-change
      (fn [& _] (h/refresh-all!)))
    {:connected-counter connected-counter_}))

(defn -main [& _]
  (h/start-app
    {:router         #'router
     :max-refresh-ms 100
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
