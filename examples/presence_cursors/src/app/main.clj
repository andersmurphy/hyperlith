(ns app.main
  (:gen-class)
  (:require [hyperlith.core :as h]))

(def css
  (h/static-css
    [["*, *::before, *::after"
      {:box-sizing :border-box
       :margin     0
       :padding    0}]

     [:.cursor-area
      {:user-select :none
       :height      :100dvh
       :width       "100%"}]

     [:.cursor
      {:position   :absolute
       :transition "all 0.2s ease-in-out"}]]))

(def cursors
  (h/cache
    (fn [db]
      (for [[sid [x y]] @db]
        [:div.cursor
         {:id    (h/digest sid)
          :style {:left (str x "px") :top (str y "px")}}
         "🚀"]))))

(defn render-home [{:keys [db] :as _req}]
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}]
    [:main#morph.main {:data-signals-x__ifmissing 0
                       :data-signals-y__ifmissing 0}
     [:div.cursor-area
      {:data-on-mousemove__debounce.100ms
       "$x = evt.clientX; $y = evt.clientY; @post('/position')"}
      (cursors db)]]))

(defn action-user-cursor-position [{:keys [sid db] {:keys [x y]} :body}]
  (when (and x y)
    (swap! db assoc sid [x y])))

(def default-shim-handler
  (h/shim-handler
    (h/html
      [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}])))

(def router
  (h/router
    {[:get (css :path)]  (css :handler)
     [:get  "/"]         default-shim-handler
     [:post "/"]         (h/render-handler #'render-home
                           :on-close
                           (fn [{:keys [sid db]}] (swap! db dissoc sid)))
     [:post "/position"] (h/action-handler action-user-cursor-position)}))

(defn state-start []
  (let [db_ (atom {})]
    (add-watch db_ :refresh-on-change h/refresh-all!)
    {:db db_}))

(defn -main [& _]
  (h/start-app
    {:router         #'router
     :max-refresh-ms 100
     :state-start       state-start
     :state-stop        (fn [_db] nil)
     :csrf-secret    (h/env :csrf-secret)}))

;; Refresh app when you re-eval file
(h/refresh-all!)

(comment
  (def server (-main))
  ;; (clojure.java.browse/browse-url "http://localhost:8080/")

  ;; stop server
  (let [stop (server :stop)] (stop))

  @(server :db)

  (reset! (server :db) {})

  ;; Example backend driven cursor test
  (doseq [_x (range 10000)]
    (Thread/sleep 1)
    (action-user-cursor-position
      {:db   (server :db)
       :sid  (rand-nth (range 1000))
       :body {:x (rand-nth (range 1 400 20))
              :y (rand-nth (range 1 400 20))}}))
  
  ,)
