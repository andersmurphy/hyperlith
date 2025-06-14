(ns app.main
  (:gen-class)
  (:require [clojure.pprint :as pprint]
            [hyperlith.core :as h]            
            [app.game :as game]))

(def board-size 50)

(def colors
  [:red :blue :green :orange :fuchsia :purple])

(def css
  (let [black           :black
        cell-transition "background 0.6s ease"]
    (h/static-css
      [["*, *::before, *::after"
        {:box-sizing :border-box
         :margin     0
         :padding    0}]

       [:html
        {:font-family "Arial, Helvetica, sans-serif"
         :font-size   :18px
         :color       black}]

       [:.main
        {:height         :100dvh
         :width          "min(100% - 2rem , 30rem)"
         :margin-inline  :auto
         :padding-block  :2dvh
         :display        :flex
         :gap            :5px
         :flex-direction :column}]

       [:.board
        {:background            :white
         :width                 "min(100% - 2rem , 30rem)"
         :display               :grid
         :aspect-ratio          "1/1"
         :grid-template-rows    (str "repeat(" board-size ", 1fr)")
         :grid-template-columns (str "repeat(" board-size ", 1fr)")}]

       [:.tile
        {:border-bottom "1px solid black"
         :border-right  "1px solid black"}]

       [:.dead
        {:background :white}]

       [:.red
        {:background :red
         :transition cell-transition}]
       [:.blue
        {:background :blue
         :transition cell-transition}]
       [:.green
        {:background :green
         :transition cell-transition}]
       [:.orange
        {:background :orange
         :transition cell-transition}]
       [:.fuchsia
        {:background :fuchsia
         :transition cell-transition}]
       [:.purple
        {:background :purple
         :transition cell-transition}]])))

(def board-state
  (h/cache
    (fn [db]
      (into []
        (comp
          (map-indexed
            (fn [id color-class]
              (let [morph-id (when-not (= :dead color-class) id)]
                (h/html
                  [:div.tile
                   {:class   color-class
                    :data-id id
                    :id      morph-id}])))))
        (:board db)))))

(defn board [snapshot]
  (let [view (board-state snapshot)]
    (h/html
      [:div {:data-on-pointerdown "@post(`/tap?id=${evt.target.dataset.id}`)"}
       [:div.board {:data-ignore true}
        view]])))

(defn render-home [{:keys [db _sid] :as _req}]
  (let [snapshot @db]
    (h/html
      [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}]
      [:main#morph.main
       [:h1 "Game of Life (multiplayer)"]
       [:p "Built with ❤️ using "
        [:a {:href "https://clojure.org/"} "Clojure"]
        " and "
        [:a {:href "https://data-star.dev"} "Datastar"]
        "🚀"]
       [:p "Source code can be found "
        [:a {:href "https://github.com/andersmurphy/hyperlith/blob/master/examples/game_of_life/src/app/main.clj"} "here"]]
       (board snapshot)])))

(defn render-home-embed [{:keys [db _sid] :as _req}]
  (let [snapshot @db]
    (h/html
      [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}]
      [:main#morph.main
       [:h1 "Game of Life (multiplayer)"]
       [:p "Built with ❤️ using Clojure and Datastar 🚀"]
       (board snapshot)])))

(defn fill-cell [board color id]
  (if ;; crude overflow check
      (<= 0 id (dec (* board-size board-size)))
    (assoc board id color)
    board))

(defn fill-cross [db id sid]
  (let[user-color (h/modulo-pick colors sid)]
    (-> db
      (update :board fill-cell user-color (- id board-size))
      (update :board fill-cell user-color (- id 1))
      (update :board fill-cell user-color id)
      (update :board fill-cell user-color (+ id 1))
      (update :board fill-cell user-color (+ id board-size)))))

(defn action-tap-cell [{:keys [sid db] {:strs [id]} :query-params}]
  (when id
    (swap! db fill-cross (parse-long id) sid)))

(def default-shim-handler
  (h/shim-handler
    (h/html
      [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}]
      [:title nil "Game of Life"]
      [:meta {:content "Conway's Game of Life" :name "description"}])))

(defn next-gen-board [current-board]
  (game/next-gen-board
    {:board    current-board
     :max-rows board-size
     :max-cols board-size}))

(defn next-generation! [db]
  (swap! db update :board next-gen-board))

(defn start-game! [db]
  (let [running_ (atom true)]
    (h/thread
      (while @running_
        (Thread/sleep 200) ;; 5 fps
        (next-generation! db)))
    (fn stop-game! [] (reset! running_ false))))

(def router
  (h/router
    {[:get (css :path)] (css :handler)
     [:get  "/"]        default-shim-handler
     [:post "/"]        (h/render-handler #'render-home
                          {:br-window-size 18})
     [:get  "/embed"]   default-shim-handler
     [:post "/embed"]   (h/render-handler #'render-home-embed
                          {:br-window-size 18})
     [:post "/tap"]     (h/action-handler #'action-tap-cell)}))

(defn ctx-start []
  (let [db_ (atom {:board (game/empty-board board-size board-size)
                   :users {}})]
    (add-watch db_ :refresh-on-change
      (fn [_ _ old-state new-state]
        ;; Only refresh if state has changed
        (when-not (= old-state new-state)
          (h/refresh-all!))))
    {:db        db_
     :game-stop (start-game! db_)}))

(defn -main [& _]
  (h/start-app
    {:router         #'router
     :max-refresh-ms 200
     :ctx-start      ctx-start
     :ctx-stop       (fn [{:keys [game-stop]}] (game-stop))
     :csrf-secret    (h/env :csrf-secret)
     :on-error       (fn [_ctx {:keys [req error]}]
                       ;; (pprint/pprint req)
                       (pprint/pprint error)
                       (flush))}))

;; Refresh app when you re-eval file
(h/refresh-all!)

(comment
  (-main)
  ;; (clojure.java.browse/browse-url "http://localhost:8080/")

  ;; stop server
  (((h/get-app) :stop))

  (def db (-> (h/get-app) :ctx :db))

  (reset! db {:board (game/empty-board board-size board-size)
              :users {}})

  (->> @db :users)

  (->> @db :board (remove false?))

  ,)
