(ns app.main
  (:gen-class)
  (:require
   [app.game :as game]
   [hyperlith.core :as h :refer [defaction defview]]
   [hyperlith.impl.lane-context :as lc]) 
  (:import
   [java.nio ByteBuffer]))

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
          [:div {:class (str (name color-class) " tile") :data-id id}])))
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

(defaction handler-tap-cell [{:keys [::h/tx! sid] {:strs [id]} :query-params}]
  (when id
    (tx! (fn [db] (swap! db fill-cross (parse-long id) sid)))))

(def shim-headers
  [[:link {:id "css" :rel "stylesheet" :type "text/css" :href css}]
   [:title "Game of Life"]
   [:meta {:content "Conway's Game of Life" :name "description"}]])

(defn board [snapshot html-ctx]
  (let [view (board-state snapshot)]
    (-> [:div {:data-on:pointerdown
               (str "@post(`" handler-tap-cell "?id=${evt.target.dataset.id}`)")}
         [:div {:class "board"} view]]
      (h/html->bytes html-ctx (.html-dst-buf html-ctx)))))

(defview render-home {:path        "/" :shim-headers shim-headers
                      :zstd-window 20}
  [{:keys [board-cache _sid] :as _req}]
  [[:link {:id "css" :rel "stylesheet" :type "text/css" :href css}]
   [:main {:class "main" :id "morph"}
    [:h1 "Game of Life (multiplayer)"]
    [:p "Built with ❤️ using "
     [:a {:href "https://clojure.org/"} "Clojure"]
     " and "
     [:a {:href "https://data-star.dev"} "Datastar"]
     "🚀"]
    [:p "Source code can be found "
     [:a {:href "https://github.com/andersmurphy/hyperlith/blob/master/examples/game_of_life/src/app/main.clj"} "here"]
     " - " [:a {:href "https://andersmurphy.com/about"} "blog"]]
    @board-cache]])

(defview render-home-embed {:path "/embed" :shim-headers shim-headers}
  [{:keys [board-cache _sid] :as _req}]
  [[:link {:id "css" :rel "stylesheet" :type "text/css" :href css}]
   [:main {:class "main" :id "morph"}
    [:h1 "Game of Life (multiplayer)"]
    [:p "Built with ❤️ using Clojure and Datastar 🚀"]
    @board-cache]])

(defn next-gen-board [current-board]
  (game/next-gen-board
    {:board    current-board
     :max-rows board-size
     :max-cols board-size}))

(defn next-generation! [db]
  (swap! db update :board next-gen-board))

(defn batch-fn [{:keys [db board-cache html-ctx]} thunks]
  (run! (fn [thunk] (thunk db)) thunks)
  (next-generation! db)
  (reset! board-cache (board @db html-ctx)))

(defn ctx-start []
  (let [db_         (atom {:board (game/empty-board board-size board-size)
                           :users {}})
        board-cache (atom nil)]
    {:board-cache board-cache
     :db          db_
     :html-ctx    (lc/new-lane-ctx
                    {:html-dst-buf (ByteBuffer/allocate (* 10 16384))})}))

(defonce app_ (atom nil))

(defn start-app! [& {:keys [dev?]}]
  (reset! app_
    (h/start-app
      {:ctx-start     ctx-start
       :batch-fn      #'batch-fn
       :batch-tick-ms 200
       :email         (h/env :email)
       :domain        (h/env :domain)
       :dev?          dev?
       :dbs {}})))

(defn -main [& _]
  (start-app!))

(comment
  (do (start-app! :dev? true) nil)
  ;; (clojure.java.browse/browse-url "http://localhost:8080/")

  ;; stop server
  ((@app_ :stop!))

  (def db (-> @app_ :ctx :db))

  (reset! db {:board (game/empty-board board-size board-size)
              :users {}})

  (->> @db :users)

  (->> @db :board (remove false?))

  ,)

(comment
  (require '[clj-async-profiler.core :as prof])

  (prof/start {:event :alloc})
  (prof/start)
  (prof/stop)
  (prof/serve-ui 7777)
  ;; (clojure.java.browse/browse-url "http://localhost:7777/")
  )
