(ns app.main
  (:gen-class)
  (:require [hyperlith.core :as h]
            [hyperlith.extras.sqlite :as d]
            [deed.core :as deed]))

;; (* 99 99 32 32)     10 036 224
;; (* 313 313 32 32)  100 320 256
;; (* 989 989 32 32) 1001 595 904

(def board-size 989 #_313 #_99)
(def chunk-size 32)
(def board-size-px (* 3 3 110000))

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
         :width          "min(100% - 2rem , 64rem)"
         :gap            :5px
         :flex-direction :column}]

       [:.view
        {:overflow        :scroll
         :overflow-anchor :none
         :width           "min(100% - 2rem , 64rem)"
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
       {:data-ignore true
        :id      chunk-id
        :data-id chunk-id
        :style   {:grid-column x :grid-row y}}
       (into []
         (map-indexed (fn [local-id box] (Checkbox local-id box)))
         chunk-cells)])))

(defn UserView [{:keys [x y] :or {x 0 y 0}} db]
  (->> (d/q db
         {:select [:id :chunk]
          :from   :chunk
          :where  [:in :id (xy->chunk-ids x y)]})
    (mapv (fn [[id chunk]]
            (Chunk id (deed/decode-from chunk))))))

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
         [:p "(actually 1,001,595,904)"]
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
        (fn action-tap-cell-thunk [db chunk-cache]
          (let [[checks] (d/q db {:select [:checks]
                                  :from   :session
                                  :where  [:= :id sid]})]
            (if checks
              (d/q db {:update :session
                       :set    {:checks (inc checks)}
                       :where  [:= :id sid]})
              (d/q db {:insert-into :session
                       :values      [{:id sid :checks 1}]})))
          (let [chunk (or (@chunk-cache chunk-id)
                        (-> (d/q db {:select [:chunk]
                                     :from   :chunk
                                     :where  [:= :id chunk-id]})
                          first
                          deed/decode-from))]
            (swap! chunk-cache assoc chunk-id
              (update chunk cell-id #(if (= 0 %) user-color 0)))))))))

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

(def blank-encoded-chunk
  (-> (repeat (* chunk-size chunk-size) 0)
    vec
    deed/encode-to-bytes))

(defn initial-board-db-state! [db]
  (let [board-range (range board-size)]
    (d/with-write-tx [db db]
      (run!
        (fn [y]
          (run! (fn [x]
                  (d/q db
                    {:insert-into :chunk
                     :values      [{:id    (xy->chunk-id x y)
                                    :chunk blank-encoded-chunk}]}))
            board-range)
          (print ".") (flush))
        board-range)))
  nil)

(defn migrations [db]
  ;; Note: all this code must be idempotent
  ;; Create tables
  (println "Running migrations...")
  (d/q db
    "CREATE TABLE IF NOT EXISTS chunk(id INT PRIMARY KEY, chunk BLOB)")
  (d/q db
    "CREATE TABLE IF NOT EXISTS session(id TEXT PRIMARY KEY, checks INTEGER) WITHOUT ROWID")
  (when-not (d/q db {:select [:id] :from :chunk :limit 1})
    (initial-board-db-state! db)))

(defn ctx-start []
  (let [tab-state_ (atom {:users {}})
        {:keys [writer reader]}
        (d/init-db! "database-new.db"
          {:pool-size 4
           :pragma    {:foreign_keys false}})
        {old-reader :reader}
        (d/init-db! "database.db"
          {:pool-size 4
           :pragma    {:foreign_keys false}})]
    ;; Run migrations
    (migrations writer)
    ;; Watch tab state
    (add-watch tab-state_ :refresh-on-change
      (fn [_ _ _ _] (h/refresh-all!)))
    {:tab           tab-state_
     :db            reader
     :db-read       reader
     :db-write      writer
     :db-old-reader old-reader
     :tx-batch!     (h/batch!
                  (fn [thunks]
                    #_{:clj-kondo/ignore [:unresolved-symbol]}
                    (let [chunk-cache (atom {})]
                      (d/with-write-tx [db writer]
                        (run! (fn [thunk] (thunk db chunk-cache)) thunks)
                        (run! (fn [[chunk-id new-chunk]]
                                (d/q db
                                  {:update :chunk
                                   :set    {:chunk
                                            (deed/encode-to-bytes new-chunk)}
                                   :where  [:= :id chunk-id]}))
                          @chunk-cache)))
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
  (d/pragma-check db)

  (UserView {:x 1 :y 1} db)

  ;; Execution time mean : 120.871429 ms
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
  
  ;; Execution time mean : 127.688298 µs
  (user/bench (do (UserView {:x 1 :y 1} db) nil))

  (d/q db {:select [[[:count :*]]] :from :session})
  (d/q db {:select [[[:sum :checks]]] :from :session})
  (d/q db {:select   [:checks] :from :session
           :order-by [[:checks :desc]]})

  (d/table-info db :chunks)
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

(comment
  (def tx-batch! (-> (h/get-app) :ctx :tx-batch!))

  (future
    (time
      (run!
        (fn [_]
          (run!
            (fn [_]
              (action-tap-cell
                {:sid          "test-user"
                 :tx-batch!    tx-batch!
                 :query-params {"pid" "0"
                                "id"  (str (rand-int 200))}}))
            ;; 10000r/s
            (range 10))
          (Thread/sleep 1))
        (range 10000))))
  )

(comment ;; migration
  (def old-db (-> (h/get-app) :ctx :db-old-reader))
  (def new-db (-> (h/get-app) :ctx :db-write))

  (defn xy->chunk-ids-old [x y]
    (-> (for [x (range x (+ x 2))
              y (range y (+ y 2))]
          (xy->chunk-id x y))
      vec))

  (let [[x y] (chunk-id->xy 0)
        [a b c d] (->> (d/q old-db
                         {:select   [[[:json_group_array :state] :chunk-cells]]
                          :from     :cell
                          :where    [:in :chunk-id (xy->chunk-ids-old (* x 2) (* y 2))]
                          :group-by [:chunk-id]})
                    (mapv #(->> % h/json->edn (partition-all 16) (mapv vec))))]
    [a b c d]
    )
  
  (run!
    (fn [id]
      (let [[x y]     (chunk-id->xy id)
            [a b c d] (->> (d/q old-db
                             {:select   [[[:json_group_array :state] :chunk-cells]]
                              :from     :cell
                              :where    [:in :chunk-id (xy->chunk-ids-old (* x 2) (* y 2))]
                              :group-by [:chunk-id]})
                        (mapv #(->> % h/json->edn (partition-all 16) (mapv vec))))
            chunk     (-> (into (vec (mapcat into a b)) (vec (mapcat into c d))))]
        (when (not= chunk blank-encoded-chunk)
          (d/q new-db
            {:update :chunk
             :set    {:chunk (deed/encode-to-bytes chunk)}
             :where  [:= :id id]}))))
    (range (* board-size board-size)))

  
  )

;; TODO: make scroll bars always visible
