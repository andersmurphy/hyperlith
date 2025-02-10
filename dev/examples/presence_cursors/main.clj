(ns examples.presence-cursors.main
  (:gen-class)
  (:require [hyperlith.core :as h]))

(def css
  (h/static-css
    [["*, *::before, *::after"
      {:box-sizing :border-box
       :margin     0
       :padding    0}]

     [:.cursor-area
      {:position :adsolute
       :height   :100dvh
       :width    "100%"}]]))

;; TODO: animate between positions

(defn render-home [{:keys [db] :as _req}]
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}]
    [:main#morph.main {:data-signals-x__ifmissing 0
                       :data-signals-y__ifmissing 0}
     [:div.cursor-area
      {:data-on-mousemove__debounce.100ms
       "$x = evt.clientX; $y = evt.clientY; @post('/position')"}
      (for [[id [x y]] @db]
        [:div
         {:id    (h/digest id)
          :style {:position  :absolute
                  :left      (str x "px")
                  :top       (str y "px")}}
         [:p "🚀"]])]]))

(defn action-user-cursor-position [{:keys [sid db] {:keys [x y]} :body}]
  (when (and x y)
    (swap! db assoc sid [x y])))

(def default-shim-handler
  (h/shim-handler
    (h/html [:link#css])))

(def router
  (h/router
    {[:get (css :path)]  (css :handler)
     [:get "/"]          default-shim-handler
     [:post "/updates"]  (h/render-handler #'render-home
                          :on-close
                          (fn [{:keys [sid db]}] (swap! db dissoc sid)))
     [:post "/position"] (h/action-handler action-user-cursor-position)}))

(defn db-start []
  (let [db_ (atom {})]
    (add-watch db_ :refresh-on-change (fn [& _] (h/refresh-all!)))
    db_))

(defn -main [& _]
  (h/start-app
    {:router         #'router
     :max-refresh-ms 100
     :db-start       db-start
     :db-stop        (fn [_db] nil)
     :csrf-secret    "fb1704df2b3484223cb5d2a79bf06a508311d8d0f03c68e724d555b6b605966d0ebb8dc54615f8d080e5fa062bd3b5bce5b6ba7ded23333bbd55deea3149b9d5"}))

;; Refresh app when you re-eval file
(h/refresh-all!)

(comment
  (def server (-main))
  ;; (clojure.java.browse/browse-url "http://localhost:8080/")

  ;; stop server
  (let [stop (server :stop)] (stop))

  @(server :db)
  
  ,)

