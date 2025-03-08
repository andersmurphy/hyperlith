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
       :overflow    :hidden
       :background  :#212529
       :color       :#e9ecef}]

     [:.main
      {:margin-top  :20px
       :display     :grid
       :place-items :center
       :gap         :2px}]

     [:.board
      {:user-select           :none
       :-webkit-touch-callout :none
       :-webkit-user-select   :none
       :width                 :100%
       :max-width             :500px
       :aspect-ratio          "1 / 1"
       :position              :relative}]

     [:.star
      {:position     :absolute
       :touch-action :none
       :font-size    :30px
       :transition   "all 0.2s ease-in-out"}]

     [:.dropzone
      {:position  :absolute
       :font-size :30px}]

     [:.counter
      {:font-size :16px}]

     [:a {:color :#e9ecef}]]))

(defn place-stars [db n]
  (doseq [_n (range n)]
    (let [x (rand-nth (range 0 100 10))
          y (rand-nth (range 0 100 10))]
      (swap! db h/assoc-in-if-missing [:stars (str "s" x y)]
        {:x x :y y}))))

(def stars
  (h/cache
    (fn [db]
      (for [[star-id {:keys [x y]}] (:stars @db)]
        [:div.star
         {:id        star-id
          :style     {:left (str x "%") :top (str y "%")}
          :draggable "true"
          :data-on-dragstart
          "evt.dataTransfer.setData('text/plain', evt.target.id)"}
         "⭐"]))))

(defn render-home [{:keys [db] :as _req}]
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}]
    [:main#morph.main
     [:p.counter "DRAG THE STARS TO THE SHIP"]
     [:p "(multiplayer co-op)"]
     [:div.board nil (stars db)
      [:div.dropzone
       {:style            {:left :55% :top :55%}
        :data-on-dragover "evt.preventDefault()"
        :data-on-drop
        "evt.preventDefault(); @post(`/dropzone?id=${evt.dataTransfer.getData('text/plain')}`)"}
       "🚀"]]
     [:p.counter nil
      (str "STARS COLLECTED: "  (@db :stars-collected))]
     [:a {:href "https://data-star.dev/"}
      "Built with ❤️ using Datastar"]
     [:a {:href "https://github.com/andersmurphy/hyperlith/blob/master/examples/drag_drop/src/app/main.clj"}
      "show me the code"]]))

(defn remove-star [db id]
  (-> (update db :stars dissoc id)
    (update :stars-collected inc)))

(defn move-star [db id]
  (swap! db assoc-in [:stars id] {:x 55 :y 55})
  (Thread/sleep 250)
  (swap! db remove-star id))

(defn action-user-move-star-to-dropzone
  [{:keys [db] {:strs [id]} :query-params}]
  (when id
    (move-star db id)))

(def default-shim-handler
  (h/shim-handler
    (h/html
      ;; Setting the colour here prevents flash on remote stylesheet update
      [:style "html {background: #212529}"]
      [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}])))

(def router
  (h/router
    {[:get (css :path)]  (css :handler)
     [:get  "/"]         default-shim-handler
     [:post "/"]         (h/render-handler #'render-home
                           :on-close
                           (fn [{:keys [sid db]}]
                             (swap! db update :cursors dissoc sid)))
     [:post "/dropzone"] (h/action-handler #'action-user-move-star-to-dropzone)}))

(defn state-start []
  (let [db_ (atom {:stars-collected 0})]
    (place-stars db_ 15)
    (add-watch db_ :refresh-on-change h/refresh-all!)
    {:db db_}))

(defn on-refresh [_ ref _old-state new-state]
  (when (empty? (:stars new-state))
    (place-stars ref 15)))

(defn -main [& _]
  (h/start-app
    {:router         #'router
     :max-refresh-ms 100
     :on-refresh     #'on-refresh
     :state-start    state-start
     :state-stop     (fn [_] nil)
     :csrf-secret    (h/env :csrf-secret)}))

;; Refresh app when you re-eval file
(h/refresh-all!)

(comment
  (def server (-main))
  ;; (clojure.java.browse/browse-url "http://localhost:8080/")

  ;; stop server
  (let [stop (server :stop)] (stop))

  (:db (server :state))

  (reset! (server :state) {})

  (place-stars (:db (server :state)) 10)

  ,)

(comment
  (def db (:db (server :state)))
  (declare db)


  (def db (:db (server :state)))
  (place-stars (:db (server :state)) 60)
  (do (mapv
        (fn [[k _]]
          (action-user-move-star-to-dropzone
            {:db           db
             :query-params {"id" k}}))
        (:stars @db))
      nil)

  

  )
