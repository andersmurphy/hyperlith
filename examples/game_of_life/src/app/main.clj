(ns app.main
  (:gen-class)
  (:require [clojure.pprint :as pprint]
            [hyperlith.core :as h :refer [defaction defview]]
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
                (h/html [:div.tile {:class   color-class :data-id id}]))))
        (:board db)))))

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

(defaction handler-tap-cell [{:keys [sid db] {:strs [id]} :query-params}]
  (when id
    (swap! db fill-cross (parse-long id) sid)))

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
  [{:keys [db _sid] :as _req}]
  (let [snapshot @db]
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
        [:a {:href "https://github.com/andersmurphy/hyperlith/blob/master/examples/game_of_life/src/app/main.clj"} "here"]]
       (board snapshot)])))

(defview render-home-embed {:path "/embed" :shim-headers shim-headers}
  [{:keys [db _sid] :as _req}]
  (let [snapshot @db]
    (h/html
      [:link#css {:rel "stylesheet" :type "text/css" :href css}]
      [:main#morph.main
       [:h1 "Game of Life (multiplayer)"]
       [:p "Built with ❤️ using Clojure and Datastar 🚀"]
       (board snapshot)])))

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
    {:max-refresh-ms 200
     :ctx-start      ctx-start
     :ctx-stop       (fn [{:keys [game-stop]}] (game-stop))
     :csrf-secret    (h/env :csrf-secret)
     :on-error       (fn [_ctx {:keys [_req error]}]
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
