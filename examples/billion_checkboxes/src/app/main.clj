(ns app.main
  (:gen-class)
  (:require [clojure.pprint :as pprint]
            [hyperlith.core :as h]
            [hyperlith.extras.sqlite :as d]))

;; (* 198 198 16 16)  10 036 224
;; (* 625 625 16 16) 100 000 000
;; (* 1977 1977 16 16) 1 000 583 424

(def board-size #_1977 #_625 198)
(def chunk-size 16)
(def board-size-px (* #_3 #_3 120000))
(def view-size 3)

(def states
  [0 1 2 3 4 5 6])

(def state->class
  ["none" "r" "b" "g" "o" "f" "p"])

(def css
  (let [black         :black
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
         :margin-inline  :auto
         :padding-block  :2dvh
         :display        :flex
         :width          "min(100% - 2rem , 42rem)"
         :gap            :5px
         :flex-direction :column}]

       [:.view
        {:overflow        :scroll
         :overflow-anchor :none
         :width           "min(100% - 2rem , 42rem)"
         :aspect-ratio    "1/1"}]

       [:.board
        {:background            :white
         :width                 board-size-px
         :display               :grid
         :aspect-ratio          "1/1"
         :gap                   :10px
         :grid-template-rows    (str "repeat(" board-size ", 1fr)")
         :grid-template-columns (str "repeat(" board-size ", 1fr)")}]

       [:.chunk
        {:background            :white
         :display               :grid
         :gap                   :10px
         :grid-template-rows    (str "repeat(" chunk-size ", 1fr)")
         :grid-template-columns (str "repeat(" chunk-size ", 1fr)")}]

       ["input[type=\"checkbox\"]"
        {:appearance    :none
         :margin        0
         :font          :inherit
         :color         :currentColor
         :border        "0.15em solid currentColor"
         :border-radius :0.15em
         :display       :grid
         :place-content :center}]

       ["input[type=\"checkbox\"]:checked::before"
        {:content    "\"\""
         :width      "0.80em"
         :height     "0.80em"
         :clip-path  "polygon(14% 44%, 0 65%, 50% 100%, 100% 16%, 80% 0%, 43% 62%)"
         :box-shadow "inset 1em 1em white"}]

       [:.pop
        {:transform  "scale(0.8)"
         :transition "scale 0.6s ease"}]
       
       [:.r {:background-color :red}]
       [:.b {:background-color :blue}]
       [:.g {:background-color :green}]
       [:.o {:background-color :orange}]
       [:.f {:background-color :fuchsia}]
       [:.p {:background-color :purple}]])))

(defn Checkbox [local-id state]
  (let [checked     (not= state 0)
        color-class (state->class state)]
    (h/html
      [:input
       {:class   (when checked color-class)
        :type    "checkbox"
        :checked checked
        :data-id local-id}])))

(defn chunk-id->xy [chunk-id]
  [(rem chunk-id board-size)
   (quot chunk-id board-size)])

(defn xy->chunk-id [x y]
  (+ x (* y board-size)))

(defn xy->chunk-ids [x y]
  (-> (for [x (range x (+ x 3))
            y (range y (+ y 3))]
        (xy->chunk-id x y))
    vec))

(defn Chunk [chunk-id chunk-cells]
  (let [[x y] (chunk-id->xy chunk-id)
        x     (inc x)
        y     (inc y)]
    (h/html
      [:div.chunk
       {:id      chunk-id
        :data-id chunk-id
        :style   {:grid-column x :grid-row y}}
       (into []
         (map-indexed (fn [local-id box] (Checkbox local-id box)))
         chunk-cells)])))

(defn UserView [{:keys [x y] :or {x 0 y 0}} db]
  (->> (d/q db
         {:select   [:chunk-id [[:json_group_array :state] :chunk-cells]]
          :from     :cell
          :where    [:in :chunk-id (xy->chunk-ids x y)]
          :group-by [:chunk-id]})
    (mapv (fn [[chunk-id chunk-cells]]
            (Chunk chunk-id (h/json->edn chunk-cells))))))

(def mouse-down-js
  (str
    "evt.target.parentElement.dataset.id &&"
    "(evt.target.classList.add('pop'),"
    "@post(`/tap?id=${evt.target.dataset.id}&pid=${evt.target.parentElement.dataset.id}`))"))

(defn Board [content]
  (h/html
    [:div#board.board
     {:data-on-mousedown mouse-down-js}
     content]))

(defn scroll-offset-js [n]
  (str "Math.round((" n "/" board-size-px ")*" board-size "-1)"))

(def on-scroll-js
  (str
    "let x = " (scroll-offset-js "el.scrollLeft") ";"
    "let y = " (scroll-offset-js "el.scrollTop") ";"
    "let change = x !== $x || y !== $y;"
    "$x = x; $y = y;"
    "change && @post(`/scroll`)"))

(defn render-home [{:keys [db sid tab tabid first-render] :as _req}]
  (let [user  (get-in @tab [sid tabid] tab)
        board (Board (UserView user db))]
    (if first-render
      (h/html
        [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}]
        [:main#morph.main {:data-signals-x "0" :data-signals-y "0"}
         [:div#view.view
          {:data-on-scroll__throttle.100ms.trail.noleading on-scroll-js}
          board]
         [:h1 "One Billion Checkboxes"]
         [:p "(actually 1,000,583,424)"]
         [:p "Built with ❤️ using "
          [:a {:href "https://clojure.org/"} "Clojure"]
          " and "
          [:a {:href "https://data-star.dev"} "Datastar"]
          "🚀"]
         [:p "Source code can be found "
          [:a {:href "https://github.com/andersmurphy/hyperlith/blob/master/examples/billion_checkboxes/src/app/main.clj" } "here"]]])
      board)))

(defn action-tap-cell
  [{:keys            [sid tx-batch!]
    {:strs [id pid]} :query-params}]
  (when (and id pid)
    (let [user-color (h/modulo-pick (subvec states 1) sid)
          cell-id    (int (parse-long id))
          chunk-id   (int (parse-long pid))]
      (tx-batch!
        (fn action-tap-cell-thunk [db]
          (let [[checks] (d/q db {:select [:checks]
                                    :from   :session
                                    :where  [:= :id sid]})]
            (if checks
              (d/q db {:update :session
                       :set    {:checks (inc checks)}
                       :where  [:= :id sid]})
              (d/q db {:insert-into :session
                       :values      [{:id sid :checks 1}]})))
          (let [[state] (d/q db {:select [:state]
                                   :from   :cell
                                   :where
                                   [:and
                                    [:= :chunk-id chunk-id]
                                    [:= :cell-id cell-id]]})
                new-state (if (= 0  state) user-color 0)]
            (d/q db {:update :cell
                     :set    {:state new-state}
                     :where  [:and
                              [:= :chunk-id chunk-id]
                              [:= :cell-id cell-id]]})))))))

(defn action-scroll [{:keys [sid tabid tab] {:keys [x y]} :body}]
  (swap! tab
    (fn [snapshot]
      (-> snapshot
        (assoc-in [sid tabid :x] (max (int x) 0))
        (assoc-in [sid tabid :y] (max (int y) 0))))))

(def default-shim-handler
  (h/shim-handler
    (h/html
      [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}]
      [:title nil "One billion checkboxes"]
      [:meta {:content "So many checkboxes" :name "description"}])))

(def router
  (h/router
    {[:get (css :path)] (css :handler)
     [:get  "/"]        default-shim-handler
     [:post "/"]        (h/render-handler #'render-home
                          {:br-window-size 19})
     [:post "/scroll"]  (h/action-handler #'action-scroll)
     [:post "/tap"]     (h/action-handler #'action-tap-cell)}))

(defn build-chunk [x y]
  (mapv (fn [c]
          {:chunk_id (xy->chunk-id x y)
           :cell_id  c
           :state    0})
    (range (* chunk-size chunk-size))))

(defn initial-board-db-state! [db]
  (let [board-range (range board-size)]
    (d/with-write-tx [db db]
      (run!
        (fn [y]
          (run! (fn [x]
                  (d/q db
                    {:insert-into :cell
                     :values      (build-chunk x y)}))
            board-range)
          (print ".") (flush))
        board-range)))
  nil)

(defn migrations [db]
  ;; Note: all this code must be idempotent

  ;; Create tables
  (println "Running migrations...")
  (d/q db
    "CREATE TABLE IF NOT EXISTS cell(chunk_id INTEGER, cell_id INTEGER, state INTEGER, PRIMARY KEY (chunk_id, cell_id)) WITHOUT ROWID")
  (d/q db
    "CREATE TABLE IF NOT EXISTS session(id TEXT PRIMARY KEY, checks INTEGER) WITHOUT ROWID")
  ;; Populate checkboxes
  (when-not (d/q db {:select [:cell-id] :from :cell :limit 1})
    (initial-board-db-state! db)))

(defn ctx-start []
  (let [tab-state_ (atom {:users {}})
        {:keys [db-write db-read]}
        (d/init-db! "database.db"
          {:pool-size 4
           :pragma    {:foreign_keys false}})]
    ;; Run migrations
    (migrations db-write)
    ;; Watch tab state
    (add-watch tab-state_ :refresh-on-change
      (fn [_ _ _ _] (h/refresh-all!)))
    {:tab       tab-state_
     :db        db-read
     :db-read   db-read
     :db-write  db-write
     :tx-batch! (h/batch! ;; TODO: add error handling to batch
                  (fn [thunks]
                    #_{:clj-kondo/ignore [:unresolved-symbol]}
                    (d/with-write-tx [db db-write]
                      (run! (fn [thunk] (thunk db)) thunks))
                    (h/refresh-all!))
                  {:run-every-ms 100})}))

(defn ctx-stop [ctx]
  (.close (:db-write ctx))
  (.close (:db-read ctx)))

(defn -main [& _]
  (h/start-app
    {:router         #'router
     :max-refresh-ms 100
     :ctx-start      ctx-start
     :ctx-stop       ctx-stop
     :csrf-secret    (h/env :csrf-secret)
     :on-error       (fn [_ctx {:keys [_req error]}]
                       (let [{:keys [cause trace type]} error]
                         (println "")
                         (println type)
                         (println cause)
                         (println "")
                         (run! println trace))
                       (flush))}))

;; Refresh app when you re-eval file
(h/refresh-all!)

(comment
  (do (-main) nil)
  ;; (clojure.java.browse/browse-url "http://localhost:8080/")

  ;; stop server
  (((h/get-app) :stop))

  (def db (-> (h/get-app) :ctx :db))

  ,)

(comment
  (def db (-> (h/get-app) :ctx :db))

  (UserView {:x 1 :y 1} db)

  ;; Execution time mean : 456.719068 ms
  ;; Execution time mean : 218.760262 ms
  (user/bench
    (->> (mapv
           (fn [n]
             (future
               (let [n (mod n board-size)]
                 (UserView {:x n :y n} db))))
           (range 0 4000))
      (run! (fn [x] @x))))

  ;; On server test
  (time ;; simulate 1000 concurrent renders
    (->> (mapv
           (fn [n]
             (future (UserView {:x n :y n} db)))
           (range 0 1000))
      (run! (fn [x] @x))))

  ;; (user/bench (do (UserView {:x 1 :y 1} db) nil))

  (d/pragma-check db)

  (d/q db {:select [[[:count :*]]] :from :session})
  (d/q db {:select [[[:sum :checks]]] :from :session})
  (d/q db {:select   [:checks] :from :session
           :order-by [[:checks :desc]]})

  (d/table-info db :cell)
  (d/table-list db)

  (user/bench ;; Execution time mean : 455.139383 µs
    (d/q db
      ["SELECT CAST(chunk_id AS TEXT), CAST(state AS TEXT) FROM cell WHERE chunk_id IN (?, ?, ?, ?, ?, ?, ?, ?, ?)"
       1978 3955 5932 1979 3956 5933 1980 3957 5934]))

  ,)

(comment
  (user/bench
    (d/q db
      ["SELECT chunk_id, JSON_GROUP_ARRAY(state) AS chunk_cells FROM cell WHERE chunk_id IN (?, ?, ?, ?, ?, ?, ?, ?, ?)  GROUP BY chunk_id" 1978 3955 5932 1979 3956 5933 1980 3957 5934]))

  (def tab-state (-> (h/get-app) :ctx :tab))

  (count @tab-state)

  (def db-write (-> (h/get-app) :ctx :db-write))

  ;; Free up space (slow)
  ;; (time (d/q db-write "VACUUM"))

  ,)

;; TODO: make scroll bars always visible
