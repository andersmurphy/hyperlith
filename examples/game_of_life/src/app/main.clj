(ns app.main
  (:gen-class)
  (:require [clojure.pprint :as pprint]
            [hyperlith.core :as h]
            [app.game :as game]))

(def board-size 100)
(def board-size-px 2000)
(def chunk-size 50)

(def colors
  [:red :blue :green :orange :fuchsia :purple])

(def css
  (let [black           :black
        cell-transition "background 0.4s ease"
        board-size-px (str board-size-px "px")]
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

       [:.view
        {:margin-inline   :auto
         :overflow        :scroll
         :overflow-anchor :none
         :width           "min(100% - 2rem , 30rem)"
         :aspect-ratio    "1/1"}]

       [:.board
        {:background            :white
         :width                 board-size-px
         :display               :grid
         :aspect-ratio          "1/1"
         :grid-template-rows    (str "repeat(" board-size ", 1fr)")
         :grid-template-columns (str "repeat(" board-size ", 1fr)")}]

       [:.tile
        {:border-bottom "1px solid black"
         :border-right  "1px solid black"
         :transition    cell-transition}]

       [:.dead
        {:background :white}]

       [:.red
        {:background   :red}]
       [:.blue
        {:background   :blue}]
       [:.green
        {:background   :green}]
       [:.orange
        {:background   :orange}]
       [:.fuchsia
        {:background   :fuchsia}]
       [:.purple
        {:background   :purple}]])))

(def board-state
  (h/cache
    (fn [db]
      (into []
        (comp
          (map-indexed
            (fn [id color-class]
              (let [[x y] (game/index->coordinates id board-size)]
                (h/html
                  [:div.tile
                   {:style {:grid-row (inc x) :grid-column (inc y)}
                    :class color-class :id (str "c" id)}]))))
          (partition-all board-size)
          (map vec))
        (:board db)))))

(defn user-view [{:keys [x y] :or {x 0 y 0}} board-state]
  (reduce
    (fn [view board-row]
      (into view (subvec board-row x (min (+ x chunk-size) board-size))))
    []
    (subvec board-state y (min (+ y chunk-size) board-size))))

(defn game-view [snapshot sid]
  (let [user (get-in snapshot [:users sid])
        view (user-view user (board-state snapshot))]
    (h/html
      [:div#view.view
       {:data-on-scroll__debounce.200ms
        "@post(`/scroll?x=${el.scrollLeft}&y=${el.scrollTop}`)"}
       [:div.chunk-grid
        [:div.board
         {:data-on-mousedown "@post(`/tap?id=${evt.target.id}`)"}
         view]]])))

(defn render-home [{:keys [db sid] :as _req}]
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
       (game-view snapshot sid)])))

(defn render-home-star [{:keys [db sid] :as _req}]
  (let [snapshot @db]
    (h/html
      [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}]
      [:main#morph.main
       (game-view snapshot sid)])))

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
    (swap! db fill-cross (parse-long (subs id 1)) sid)))

(defn action-scroll [{:keys [sid db] {:strs [x y]} :query-params}]
  (swap! db
    (fn [snapshot]
      (-> snapshot
        (assoc-in [:users sid :x]
          (max (- (int (* (/ (parse-double x) board-size-px) board-size))
                 10)
            0))
        (assoc-in [:users sid :y]
          (max (- (int (* (/ (parse-double y) board-size-px) board-size))
                 10)
            0))))))

;; 9511 9511

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
     [:get  "/star"]    default-shim-handler
     [:post "/star"]    (h/render-handler #'render-home-star
                          {:br-window-size 18})
     [:post "/scroll"]  (h/action-handler #'action-scroll)
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

  (->> (h/traces) first :ret (take 10))
  (h/traces-reset!)
  (h/traces)

  ,)
