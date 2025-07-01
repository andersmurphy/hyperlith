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
       :font-size :50px}]

     [:.popover
      {:position  :absolute
       :top       :50%
       :left      :50%
       :transform "translate(-50%, -50%)"}]]))

(def shim-headers
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href css}]))

(defview render-home
  {:path "/"
   :shim-headers shim-headers
   :on-open
   (fn [{:keys [_ db]}]
     (swap! db update :connected-users inc))
   :on-close
   (fn [{:keys [_ db]}]
     (swap! db update :connected-users dec))}
  [{:keys [db] :as _req}]
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href css}]
    [:main#morph.main
     ;; We track connected users as this will cause updates out of bounds
     ;; and will show that the popover state is not affected by other users
     [:div
      [:p nil "connected users"]
      [:p.counter nil (@db :connected-users)]]
     [:button {:popovertarget "my-popover"} "Open Popover"]
     [:div#my-popover.popover {:popover true} "Greetings, one and all!"]]))

(defn ctx-start []
  (let [db_ (atom {:connected-users 0})]
    (add-watch db_ :refresh-on-change (fn [& _] (h/refresh-all!)))
    {:db db_}))

(defn -main [& _]
  (h/start-app
    {:max-refresh-ms 100
     :ctx-start       ctx-start
     :ctx-stop        (fn [_state] nil)
     :csrf-secret    (h/env :csrf-secret)}))

;; Refresh app when you re-eval file
(h/refresh-all!)

(comment
  (-main)
  ;; (clojure.java.browse/browse-url "http://localhost:8080/")

  ;; stop server
  (((h/get-app) :stop))

  ,)
