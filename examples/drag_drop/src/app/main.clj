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
      {:font-family "Arial, Helvetica, sans-serif"
       :background  :#212529
       :color       :#e9ecef}]

     [:.main
      {:margin-top :20px
       :display      :grid
       :place-items :center}]

     [:.board
      {:user-select           :none
       :-webkit-touch-callout :none
       :-webkit-user-select   :none
       :height                :400px
       :width                 :400px
       :position              :relative}]

     [:.star
      {:position    :absolute
       :font-size   :20px
       :transition  "all 0.2s ease-in-out"
       }]

     [:.dropzone
      {:position  :absolute
       :font-size :40px}]

     [:.counter
      {:font-size :20px}]

     [:a {:color       :#e9ecef}]]))

(defn place-stars [db n]
  (doseq [_n (range n)]
    (swap! db h/assoc-in-if-missing [:stars (h/new-uid)]
      {:x (rand-nth (range 0 400 20))
       :y (rand-nth (range 0 400 20))})))

(def stars
  (h/cache
    (fn [db]
      (for [[star-id {:keys [x y]}] (:stars @db)]
        [:div.star
         {:id                star-id
          :style             {:left (str x "px") :top (str y "px")}
          :draggable         "true"
          :data-on-dragstart
          "evt.dataTransfer.setData('text/plain', evt.target.id)"}
         "⭐"]))))

(defn render-home [{:keys [db] :as _req}]
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}]
    [:main#morph.main     
     [:div [:p.counter "DRAG THE STARS TO THE SHIP"]]
     [:p "(multiplayer coop)"]
     [:div.board (stars db)
      [:div.dropzone
       {:style            {:left :100px :top :100px}
        :data-on-dragover "evt.preventDefault()"
        :data-on-drop
        "evt.preventDefault(); @post(`/dropzone?id=${evt.dataTransfer.getData('text/plain')}`)"}
       "🚀"]]
     [:div [:p.counter (str "STARS COLLECTED: "  (@db :stars-collected))]]
     [:a {:href "https://data-star.dev/"}
      "Built with ❤️ using Datastar"]]))

(defn remove-star [db id]
  (-> (update db :stars dissoc id)
    (update :stars-collected inc)))

(defn action-user-dropzone [{:keys        [db]
                             {:strs [id]} :query-params}]
  (when id
    (swap! db assoc-in [:stars id] {:x 110 :y 110})
    (Thread/sleep 250)
    (swap! db remove-star id)))

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
                          (fn [{:keys [sid db]}]
                            (swap! db update :cursors dissoc sid)))
     [:post "/dropzone"] (h/action-handler #'action-user-dropzone)}))

(defn db-watcher-fn [_ ref _old-state new-state]
  (when (empty? (:stars new-state))
    (place-stars ref 20))
  (h/refresh-all!))

(defn db-start []
  (let [db_ (atom {})]
    (place-stars db_ 20)
    (add-watch db_ :refresh-on-change #'db-watcher-fn)
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

  (reset! (server :db) {})

  (place-stars (server :db) 10)

  ,)

