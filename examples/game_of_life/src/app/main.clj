(ns app.main
  (:gen-class)
  (:require [hyperlith.core :as h :refer [defaction defview]]
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

(defn board-state [db]
  (into []
    (comp
      (map-indexed
        (fn [id color-class]
          (h/html [:div.tile {:class color-class :data-id id}]))))
    (:board db)))

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

(defaction handler-tap-cell [{:keys [tx-batch! sid] {:strs [id]} :query-params}]
  (when id
    (tx-batch!
      (fn [db]
        (swap! db fill-cross (parse-long id) sid)))))

(def shim-headers
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href css}]
    [:title nil "Game of Life"]
    [:meta {:content "Conway's Game of Life" :name "description"}]))

(defn board [snapshot]
  (let [view (board-state snapshot)]
    (h/html
      [:div {:data-on-pointerdown
             (str "@post(`" handler-tap-cell "?id=${evt.target.dataset.id}`)")}
       [:div.board nil view]])))

(defview render-home {:path "/" :shim-headers shim-headers}
  [{:keys [board-cache _sid] :as _req}]
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href css}]
    [:main#morph.main
     [:h1 "Game of Life (multiplayer)"]
     [:p "Built with ❤️ using "
      [:a {:href "https://clojure.org/"} "Clojure"]
      " and "
      [:a {:href "https://data-star.dev"} "Datastar"]
      "🚀"]
     [:p "Source code can be found "
      [:a {:href "https://github.com/andersmurphy/hyperlith/blob/master/examples/game_of_life/src/game_of_life/main.clj"} "here"]]
     @board-cache]))

(defview render-home-embed {:path "/embed" :shim-headers shim-headers}
  [{:keys [board-cache _sid] :as _req}]
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href css}]
    [:main#morph.main
     [:h1 "Game of Life (multiplayer)"]
     [:p "Built with ❤️ using Clojure and Datastar 🚀"]
     @board-cache]))

(defn next-gen-board [current-board]
  (game/next-gen-board
    {:board    current-board
     :max-rows board-size
     :max-cols board-size}))

(defn next-generation! [db]
  (swap! db update :board next-gen-board))

(defn start-game! [tx-batch!]
  (let [running_ (atom true)]
    (h/thread
      (while @running_
        (Thread/sleep 200) ;; 5 fps
        (tx-batch!
          (fn [db] (next-generation! db)))))
    (fn stop-game! [] (reset! running_ false))))

(defn ctx-start []
  (let [db_         (atom {:board (game/empty-board board-size board-size)
                           :users {}})
        board-cache (atom nil)
        tx-batch!   (h/batch!
                      (fn [thunks]
                        (run! (fn [thunk] (thunk db_)) thunks)
                        ;; rebuild board state
                        (reset! board-cache (board @db_))
                        (h/refresh-all!))
                      {:run-every-ms 200})]
    {:board-cache board-cache
     :db          db_
     :tx-batch!   tx-batch!
     :game-stop   (start-game! tx-batch!)}))

(defonce app_ (atom nil))

(defn -main [& _]
  (reset! app_
    (h/start-app
      {:max-refresh-ms 200
       :ctx-start      ctx-start
       :ctx-stop       (fn [{:keys [game-stop]}] (game-stop))
       :csrf-secret    (h/env :csrf-secret)})))

;; Refresh app when you re-eval file
(h/refresh-all!)

(comment
  (do (-main) nil)
  ;; (clojure.java.browse/browse-url "http://localhost:8080/")

  ;; stop server
  ((@app_ :stop))

  (def db (-> @app_ :ctx :db))

  (reset! db {:board (game/empty-board board-size board-size)
              :users {}})

  (->> @db :users)

  (->> @db :board (remove false?))

  ,)
