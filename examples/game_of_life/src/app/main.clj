(ns app.main
  (:gen-class)
  (:require [clojure.pprint :as pprint]
            [hyperlith.core :as h]
            [app.game :as game]))

;; TODO: tune the algoritm
(def board-size 100)
(def view-size 50)

(def colors
  [:red :blue :green :orange :fuchsia :purple])

(def css
  (let [black :black]
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
        {:overflow        :scroll
         :overflow-anchor :none
         :width           "min(100% - 2rem , 30rem)"
         :aspect-ratio    "1/1"}]

       [:.board
        {:background            black
         :gap                   :1px
         :width                 :1000px
         :display               :grid
         :aspect-ratio          "1/1"
         :grid-template-rows
         (str "0.2px " "repeat("view-size", 1fr)" " 0.2px")
         :grid-template-columns
         (str "0.2px " "repeat("view-size", 1fr)" " 0.2px")}]

       [:.old-board
        {:background   black
         :gap          :1px
         :width        "min(100% - 2rem , 30rem)"
         :display      :grid
         :aspect-ratio "1/1"
         :grid-template-rows
         (str "0.2px " "repeat("view-size", 1fr)" " 0.2px")
         :grid-template-columns
         (str "0.2px " "repeat("view-size", 1fr)" " 0.2px")}]

       [:.north
        {:grid-column (str "1 /" (+ view-size 3))
         :background  :white}]

       [:.west
        {:grid-row    (str "2 /" (+ view-size 2))
         :background  :white}]

       [:.south
        {:grid-column    (str "1 /" (+ view-size 3))
         :grid-row (+ view-size 2)
         :background  :white}]

       [:.east
        {:grid-row    (str "2 /" (+ view-size 2))
         :grid-column (+ view-size 2)
         :background  :white}]

       [:.tile
        {:transition "background 0.4s ease"}]

       [:.dead
        {:background :white}]

       [:.red
        {:background   :red
         :accent-color :red}]
       [:.blue
        {:background   :blue
         :accent-color :blue}]
       [:.green
        {:background   :green
         :accent-color :green}]
       [:.orange
        {:background   :orange
         :accent-color :orange}]
       [:.fuchsia
        {:background   :fuchsia
         :accent-color :fuchsia}]
       [:.purple
        {:background   :purple
         :accent-color :purple}]])))

(def board-state
  (h/cache
    (fn [db]
      (into []
        (comp
          (map-indexed
            (fn [id color-class]
              (h/html
                [:div.tile
                 {:class   color-class
                  ;; TODO: pick :id or :data-id
                  ;; :id      (str "c" id)
                  :data-id (str "c" id)}])))
          (partition-all board-size)
          (map vec))
        (:board db)))))

;; north scroll positive shift
;; west  scroll positive shift
;; east  scroll negative shift
;; south scroll negative shift
;; hide -> move -> show

(def center-view
  "let r = el.getBoundingClientRect();
el.scrollLeft = el.scrollLeft + $_x;
el.scrollTop = el.scrollTop + $_y;")

(defn circular-subvec
  "Like subvec but loops round. Result can never be larger than the initial
  vector v."
  [v start end]
  (let [size (count v)]
    (if (>= end size)
      (let [v1 (subvec v start size)]
        (into v1 (subvec v 0 (min (- end size) (- size (count v1))))))
      (subvec v start end))))

(defn user-view [{:keys [x y] :or {x 0 y 0}} board-state]
  (reduce
    (fn [view board-row]
      (into view (circular-subvec board-row x (+ x view-size))))
    []
    (circular-subvec board-state y (+ y view-size))))

(defn render-home [{:keys [db sid] :as _req}]
  (let [snapshot @db
        user     (get-in snapshot [:users sid])
        view     (user-view user (board-state snapshot))]
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
       [:div#view.view 
        [:div.board
         {:data-on-mousedown "@post('/tap?id='+evt.target.dataset.id)"}
         [:div.north
          ;; {:data-on-intersect__once
          ;;  "@post('/scroll?id=north');"}
          ]
         [:div.west
          ;; {:data-on-intersect__once
          ;;  "@post('/scroll?id=west');"}
          ]
         view
         [:div.east
          ;; {:data-on-intersect__once
          ;;  "@post('/scroll?id=east');"}
          ]
         [:div.south
          ;; {:data-on-intersect__once
          ;;  "@post('/scroll?id=south');"}
          ]]]])))

(defn render-home-small [{:keys [db sid] :as _req}]
  (let [snapshot @db
        user     (get-in snapshot [:users sid])
        view     (user-view user (board-state snapshot))]
    (h/html
      [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}]
      [:main#morph.main
       [:div.old-board
        {:data-on-mousedown "@post('/tap?id='+evt.target.dataset.id)"}
        [:div.north]
        [:div.west]
        view
        [:div.east]
        [:div.south]]])))

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

(defn action-scroll [{:keys [sid db] {:strs [id]} :query-params}]
  (let [shift 5]
    (swap! db update-in [:users sid]
      (fn [{:keys [x y] :as user :or {x 0 y 0}}]
        (case id
          "north" (assoc user :y (mod (- y shift) board-size))
          "west"  (assoc user :x (mod (- x shift) board-size))
          "east"  (assoc user :x (mod (+ x shift) board-size))
          "south" (assoc user :y (mod (+ y shift) board-size)))))))

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
     [:post "/star"]    (h/render-handler #'render-home-small
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
  ;; Virtualize vanila

  ;; modulo wrap

  (->> @db :users)

  (->> @db :board (remove false?))

  (->> (h/traces) first :ret (take 10))
  (h/traces-reset!)
  (h/traces)

  ,)
