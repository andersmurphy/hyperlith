(ns app.main
  (:gen-class)
  (:require [hyperlith.core :as h]
            [hyperlith.extras.sqlite :as d]
            [clj-async-profiler.core :as prof]))

;; (* 198 198 16 16)  10 036 224
;; (* 625 625 16 16) 100 000 000
;; (* 1977 1977 16 16) 1 000 583 424

(def board-size 1977 #_625 #_198)
(def chunk-size 16)
(def board-size-px (* 3 3 120000))
(def size (* board-size chunk-size))

(def states
  [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14])

(def state->class
  (mapv #(str "_" %) states))

(def css
  (let [black         "#000000"
        white         "#FFF1E8"
        accent        "#FFA300"
        board-size-px (str board-size-px "px")]
    (h/static-css
      [["*, *::before, *::after"
        {:box-sizing :border-box
         :margin     0
         :padding    0}]

       [:html
        {:font-family  "Arial, Helvetica, sans-serif"
         :font-size    :1.0rem
         :color        black
         :background   white}]

       ["input[type=\"number\"]::-webkit-outer-spin-button,
         input[type=\"number\"]::-webkit-inner-spin-button"
        {:-webkit-appearance :none :margin 0}]
       ["input[type=\"number\"]" { :-moz-appearance :textfield}]

       ["::-webkit-scrollbar"
        {:background white :width :10px :height :10px}]
       ["::-webkit-scrollbar-corner" {:background white}]
       ["::-webkit-scrollbar-track" {:background white}]
       ["::-webkit-scrollbar-thumb"
        {:background    black
         :border-radius :0.15em}]

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
         :scroll-behavior :smooth
         :scrollbar-color (str black " transparent")
         :overflow-anchor :none
         :width           "min(100% - 2rem , 42rem)"
         :height          "min(100% - 2rem , 42rem)"}]

       [:.board
        {:background            white
         :width                 board-size-px
         :display               :grid
         :aspect-ratio          "1/1"
         :gap                   :10px
         :grid-template-rows    (str "repeat(" board-size ", 1fr)")
         :grid-template-columns (str "repeat(" board-size ", 1fr)")
         :pointer-events        :none}]

       [:.chunk
        {:background            white
         :display               :grid
         :gap                   :10px
         :grid-template-rows    (str "repeat(" chunk-size ", 1fr)")
         :grid-template-columns (str "repeat(" chunk-size ", 1fr)")}]

       ["input[type=\"checkbox\"]"
        {:appearance     :none
         :font           :inherit
         :font-size      :1.2rem
         :color          :currentColor
         :border         "0.15em solid currentColor"
         :border-radius  :0.15em
         :display        :grid
         :place-content  :center
         :pointer-events :all}]

       ["input[type=\"checkbox\"]:checked::before"
        {:content    "\"\""
         :width      "0.50em"
         :height     "0.50em"
         :clip-path  "polygon(14% 44%, 0 65%, 50% 100%, 100% 16%, 80% 0%, 43% 62%)"
         :box-shadow (str "inset 1em 1em " white)}]

       [:.pop
        {:transform  "scale(0.8)"
         :transition "scale 0.6s ease"}]

       [:._1  {:background-color "#FF004D"}]
       [:._2  {:background-color "#29ADFF"}]
       [:._3  {:background-color "#00E436"}]
       [:._4  {:background-color "#FFA300"}]
       [:._5  {:background-color "#FF77A8"}]
       [:._6  {:background-color "#7E2553"}]
       [:._7  {:background-color "#FFCCAA"}]
       [:._8  {:background-color "#1D2B53"}]
       [:._9  {:background-color "#AB5236"}]
       [:._10 {:background-color "#FFEC27"}]
       [:._11 {:background-color "#008751"}]
       [:._12 {:background-color "#C2C3C7"}]
       [:._13 {:background-color "#83769C"}]
       [:._14 {:background-color "#5F574F"}]

       [:.palette
        {:margin-block          :5px
         :font-size             :1.2rem
         :background            white
         :width                 "min(100% - 2rem , 42rem)"
         :display               :grid
         :gap                   :10px
         :grid-template-columns "repeat(auto-fill, 2rem)"
         :pointer-events        :none}]

       [:.palette-item
        {:aspect-ratio   "1/1"
         :border-radius  :0.15em
         :pointer-events :all}]

       [:.palette-selected
        {:outline "0.15em solid currentColor"}]

       [:.jump
        {:display        :flex
         :gap            :5px
         :flex-direction :row
         :align-items    :center}]

       ["input:not([type=\"checkbox\"]):focus"
        {:outline       :none
         :border-radius :0.15em
         :border        (str "0.15em solid " accent)}]

       [:a {:color accent}]

       [:.jump-input
        {:background    white
         :width         :6rem
         :font-size     :1.2rem
         :border-radius :0.15em
         :border        "0.15em solid currentColor"
         :padding       :5px}]])))

(defn Checkbox [local-id state]
  (let [state       (or state 0)
        checked     (not= state 0)
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
            (Chunk id (d/decode chunk))))))

(defn Board [content]
  (h/html
    [:div#board.board
     {:data-on-mousedown
      (str
    "evt.target.classList.add('pop');" "@post(`/tap?id=${evt.target.dataset.id}&pid=${evt.target.parentElement.dataset.id}`)")}
     content]))

(defn scroll-offset-js [n]
  (str "Math.round((" n "/" board-size-px ")*" board-size "-1)"))

(defn scroll->cell-xy-js [n]
  (str "Math.round((" n "/" board-size-px ")*" size ")"))

(def scroll-jumpx-js
  (str "$view.scrollLeft= $jumpx/" size "*" board-size-px ";"))

(def scroll-jumpy-js
  (str "$view.scrollTop=  $jumpy/" size "*" board-size-px ";"))

(def on-scroll-js
  (str    
    "$jumpx = " (scroll->cell-xy-js "el.scrollLeft") ";"
    "$jumpy = " (scroll->cell-xy-js "el.scrollTop") ";"
    "let x = " (scroll-offset-js "el.scrollLeft") ";"
    "let y = " (scroll-offset-js "el.scrollTop") ";"
    "let change = x !== $x || y !== $y;"
    "$x = x; $y = y;"
    "change && @post(`/scroll`)"))

(defn Palette [current-selected]
  (h/html
    [:div.palette
     {:data-signals-color "1"
      :data-on-mousedown
      (str
        "(evt.target.dataset.id !== $color) &&"
        "(evt.target.classList.add('pop'),"
        "$color = evt.target.dataset.id,"
        "@post(`/palette`))")}
     (mapv (fn [state]
             (h/html [:div.palette-item
                      {:data-id state
                       :class
                       (str (state->class state)
                         (when (= current-selected state)
                           " palette-selected"))}]))
       (subvec states 1))]))

(defn render-home [{:keys [db sid tab tabid] :as _req}]
  (let [user    (get-in @tab [sid tabid] tab)
        board   (Board (UserView user db))
        palette (Palette (or (:color user) 1))]
    (h/html
      [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}]
      [:main#morph.main {:data-signals-x "0" :data-signals-y "0"}
       [:div#view.view
        {;; firefox sometimes preserves scroll on refresh and we don't want that
         :data-on-load__once                             "el.scrollTo(0,0)"
         :data-ref                                       "view"
         :data-on-scroll__throttle.100ms.trail.noleading on-scroll-js}
        board]
       [:div.jump
        [:h2 "X:"]
        [:input.jump-input
         {:type                          "number" :data-bind "jumpx"
          :data-on-input__debounce.600ms scroll-jumpx-js}]
        [:h2 "Y:"]
        [:input.jump-input
         {:type                          "number" :data-bind "jumpy"
          :data-on-input__debounce.600ms scroll-jumpy-js}]]
       palette
       [:h1 "One Billion Checkboxes"]
       [:p "Built using "
        [:a {:href "https://clojure.org/"} "Clojure"]
        " and "
        [:a {:href "https://data-star.dev"} "Datastar"]
        " - "
        [:a {:href "https://github.com/andersmurphy/hyperlith/blob/master/examples/billion_checkboxes_blob/src/app/main.clj" } "source"]
        " - "
        [:a {:href "https://lospec.com/palette-list/pico-8"} "palette"]]])))

(defn action-tap-cell
  [{:keys            [sid tx-batch! tab tabid]
    {:strs [id pid]} :query-params}]
  (when (and id pid)
    (let [user-color (or (:color (get-in @tab [sid tabid] tab)) 1)
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
                          d/decode))]
            (swap! chunk-cache assoc chunk-id
              (update chunk cell-id #(if (= 0 %) user-color 0)))))))))

(defn action-scroll [{:keys [sid tabid tab] {:keys [x y]} :body}]
  (swap! tab
    (fn [snapshot]
      (-> snapshot
        (assoc-in [sid tabid :x] (max (int x) 0))
        (assoc-in [sid tabid :y] (max (int y) 0))))))

(defn action-tap-palette
  [{:keys [sid tab tabid] {:keys [color]} :body}]
  (let [color (parse-long color)]
    (when (< 0 color (count states))
      (swap! tab assoc-in [sid tabid :color] color))))

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
                          {:br-window-size 18})
     [:post "/scroll"]  (h/action-handler #'action-scroll)
     [:post "/tap"]     (h/action-handler #'action-tap-cell)
     [:post "/palette"] (h/action-handler #'action-tap-palette)}))

(def blank-encoded-chunk
  (-> (repeat (* chunk-size chunk-size) 0)
    vec
    d/encode))

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
           :pragma    {:foreign_keys false}})]
    ;; Run migrations
    (migrations writer)
    ;; Watch tab state
    (add-watch tab-state_ :refresh-on-change
      (fn [_ _ _ _] (h/refresh-all!)))
    {:tab       tab-state_
     :db        reader
     :db-read   reader
     :db-write  writer
     :tx-batch! (h/batch!
                      (fn [thunks]
                        #_{:clj-kondo/ignore [:unresolved-symbol]}
                        (let [chunk-cache (atom {})]
                          (d/with-write-tx [db writer]
                            (run! (fn [thunk] (thunk db chunk-cache)) thunks)
                            (run! (fn [[chunk-id new-chunk]]
                                    (d/q db
                                      {:update :chunk
                                       :set    {:chunk
                                                (d/encode new-chunk)}
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
  
  ;; Execution time mean : 134.262151 ms
  (user/bench
    (->> (mapv
           (fn [n]
             (future
               (let [n (mod n board-size)]
                 (UserView {:x n :y n} db)
                 ;; we don't want to hold onto the object
                 ;; not realistic
                 nil)))
           (range 0 4000))
      (run! (fn [x] @x))))
  
  ;; Execution time mean : 158.210678 µs
  (user/bench (do (UserView {:x 1 :y 1} db) nil))

  (d/q db {:select [[[:count :*]]] :from :session})
  (d/q db {:select [[[:sum :checks]]] :from :session})
  (d/q db {:select   [:checks] :from :session
           :order-by [[:checks :desc]]})

  (d/table-info db :chunks)
  (d/table-list db)

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
  ;; Checkpoint the WAL
  (d/q db-write "PRAGMA wal_checkpoint(PASSIVE)")
  (d/q db-write "PRAGMA wal_checkpoint(TRUNCATE)")

  ,)

(comment ;; Profiling
  (prof/start)
  (prof/stop)
  (prof/serve-ui 7777)
  ;; (clojure.java.browse/browse-url "http://localhost:7777/")
  )

(comment
  (def tx-batch! (-> (h/get-app) :ctx :tx-batch!))
  (def tab (-> (h/get-app) :ctx :tab))

  (future
    (time
      (run!
        (fn [_]
          (run!
            (fn [_]
              (action-tap-cell
                {:sid          "test-user"
                 :tx-batch!    tx-batch!
                 :tab          tab
                 :query-params {"pid" "0"
                                "id"  (str (rand-int 200))}}))
            ;; 10000r/s
            (range 10))
          (Thread/sleep 1))
        (range 10000))))
  )
