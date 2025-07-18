(ns app.main
  (:gen-class)
  (:require [hyperlith.core :as h :refer [defaction defview]]
            [hyperlith.extras.sqlite :as d]
            [clj-async-profiler.core :as prof]
            [clojure.math :as math]))

(def chunk-size 16)
(def board-size (->> (math/pow chunk-size 2)
                  (/ 1000000000)
                  math/sqrt
                  math/ceil
                  int))
(def board-size-px (* 32 chunk-size board-size))
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
        {;; Animation that depresses the element
         :transform      "scale(0.8)"
         :transition     "scale 0.6s ease"
         ;; Disable element until next frame/morph
         :pointer-events :none}]

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
        {:outline        "0.15em solid currentColor"
         :pointer-events :none}]

       [:.jump
        {:display        :flex
         :gap            :5px
         :flex-direction :row
         :align-items    :center}]

       ["input[type=\"number\"]:focus"
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

(defn get-tab-state [db sid tabid]
  (-> (d/q db {:select [:state]
               :from   :tab
               :where  [:and [:= :sid sid] [:= :tabid tabid]]})
    first))

(defn upsert-tab-state! [db sid tabid update-fn]
  (let [old-state (get-tab-state db sid tabid)
        new-state (update-fn old-state)]
    (if old-state
      (d/q db {:update :tab
               :set    {:state [:lift new-state]}
               :where  [:and [:= :sid sid] [:= :tabid tabid]]})
      (d/q db {:insert-into :tab
               :values      [{:sid   sid
                              :tabid tabid
                              :state [:lift new-state]}]}))))

(defaction handler-scroll
  [{:keys [sid tabid tx-batch!] {:keys [x y]} :body}]
  (tx-batch!
    (fn [db _]
      (upsert-tab-state! db sid tabid
        #(assoc %
           :x (max (int x) 0)
           :y (max (int y) 0))))))

(defaction handler-palette
  [{:keys [sid tabid tx-batch!] {:keys [targetid]} :body}]
  (let [color (parse-long targetid)]
    (when (< 0 color (count states))
      (tx-batch!
        (fn [db _]
          (upsert-tab-state! db sid tabid #(assoc % :color color)))))))

(defaction handler-check
  [{:keys                       [sid tx-batch! db tabid]
    {:keys [targetid parentid]} :body}]
  (when (and targetid parentid)
    (let [user-color (or (:color (get-tab-state db sid tabid)) 1)
          cell-id    (int (parse-long targetid))
          chunk-id   (int (parse-long parentid))]
      (when (>= (dec (* chunk-size chunk-size)) cell-id 0)
        (tx-batch!
          (fn [db chunk-cache]
            (let [chunk (or (@chunk-cache chunk-id)
                          (-> (d/q db {:select [:chunk]
                                       :from   :chunk
                                       :where  [:= :id chunk-id]})
                            first))]
              (swap! chunk-cache assoc chunk-id
                (update chunk cell-id #(if (= 0 %) user-color 0))))))))))

(defn Checkbox [local-id state]
  (let [state       (or state 0)
        checked     (not= state 0)
        color-class (state->class state)]
    (h/html
      [:input
       {:class       (when checked color-class)
        :type        "checkbox"
        :checked     checked
        :data-id     local-id
        :data-action handler-check}])))

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
         {:select [:id :chunk]
          :from   :chunk
          :where  [:in :id (xy->chunk-ids x y)]})
    (mapv (fn [[id chunk]]
            (Chunk id chunk)))))

(defn scroll-offset-js [n]
  (str "Math.round((" n "/" board-size-px ")*" board-size "-1)"))

(defn scroll->cell-xy-js [n]
  (str "Math.round((" n "/" board-size-px ")*" size ")"))

(def scroll-jumpx-js
  (str "$_view.scrollLeft= $_jumpx/" size "*" board-size-px ";"))

(def scroll-jumpy-js
  (str "$_view.scrollTop=  $_jumpy/" size "*" board-size-px ";"))

(def on-scroll-js
  (str    
    "$_jumpx = " (scroll->cell-xy-js "el.scrollLeft") ";"
    "$_jumpy = " (scroll->cell-xy-js "el.scrollTop") ";"
    "let x = " (scroll-offset-js "el.scrollLeft") ";"
    "let y = " (scroll-offset-js "el.scrollTop") ";"
    "let change = x !== $x || y !== $y;"
    "$x = x; $y = y;"
    "change && @post(`" handler-scroll "`)"))

(defn Palette [current-selected]
  (h/html
    [:div.palette nil
     (mapv (fn [state]
             (h/html [:div.palette-item
                      {:data-id state
                       :data-action handler-palette
                       :class
                       (str (state->class state)
                         (when (= current-selected state)
                           " palette-selected"))}]))
       (subvec states 1))]))

(def shim-headers
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href css}]
    [:title nil "One billion checkboxes"]
    [:meta {:content "So many checkboxes" :name "description"}]))

(defview handler-root
  {:path "/" :shim-headers shim-headers :br-window-size 19}
  [{:keys [db sid tabid] :as _req}]
  (let [user    (get-tab-state db sid tabid)
        content (UserView user db)
        palette (Palette (or (:color user) 1))]
    (h/html
      [:link#css {:rel "stylesheet" :type "text/css" :href css}]
      [:main#morph.main
       {:data-on-mousedown
        (str
          "if (evt.target.dataset.action) {"
          "evt.target.classList.add('pop');"
          "$targetid = evt.target.dataset.id;"
          "$parentid = evt.target.parentElement.dataset.id;"
          "@post(`${evt.target.dataset.action}`);"
          "}")}
       [:div#view.view
        {;; firefox sometimes preserves scroll on refresh and we don't want that
         :data-on-load                                   "el.scrollTo(0,0)"
         :data-ref                                       "_view"
         :data-on-scroll__throttle.100ms.trail.noleading on-scroll-js}
        [:div#board.board nil content]]
       [:div.jump
        [:h2 "X:"]
        [:input.jump-input
         {:type                       "number" :data-bind "_jumpx"
          :data-on-input__debounce.1s scroll-jumpx-js}]
        [:h2 "Y:"]
        [:input.jump-input
         {:type                       "number" :data-bind "_jumpy"
          :data-on-input__debounce.1s scroll-jumpy-js}]]
       palette
       [:h1 "One Billion Checkboxes"]
       [:p "Built using "
        [:a {:href "https://clojure.org/"} "Clojure"]
        " and "
        [:a {:href "https://data-star.dev"} "Datastar"]
        " - "
        [:a {:href "https://github.com/andersmurphy/hyperlith/blob/master/examples/billion_checkboxes_blob/src/app/main.clj" } "source"]
        " - "
        [:a {:href "https://cells.andersmurphy.com"} " more like this"]]])))

(def blank-chunk
  (-> (repeat (* chunk-size chunk-size) 0)
    vec))

(defn initial-board-db-state! [db]
  (let [board-range (range board-size)]
    (d/with-write-tx [db db]
      (run!
        (fn [y]
          (run! (fn [x]
                  (d/q db
                    {:insert-into :chunk
                     :values      [{:id    (xy->chunk-id x y)
                                    :chunk [:lift blank-chunk]}]}))
            board-range)
          (print ".") (flush))
        board-range)))
  nil)

(defn migrations [db]
  ;; Note: all this code must be idempotent
  ;; Create tables
  (println "Running migrations...")
  (d/q db
    ["CREATE TABLE IF NOT EXISTS chunk(id INT PRIMARY KEY, chunk BLOB)"])
  (d/q db
    ["CREATE TABLE IF NOT EXISTS tab(sid TEXT, tabid TEXT, state BLOB, PRIMARY KEY (sid, tabid)) WITHOUT ROWID"])
  (when-not (d/q db {:select [:id] :from :chunk :limit 1})
    (initial-board-db-state! db)))

(defn ctx-start []
  (let [{:keys [writer reader]}
        (d/init-db! "database-new.db"
          {:pool-size 4
           :pragma    {:foreign_keys false}})]
    ;; Run migrations
    (migrations writer)
    {:db        reader
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
                                   :set    {:chunk [:lift new-chunk]}
                                   :where  [:= :id chunk-id]}))
                          @chunk-cache)))
                    (h/refresh-all!))
                  {:run-every-ms 100})}))

(defn ctx-stop [ctx]
  (.close (:db-write ctx))
  (.close (:db-read ctx)))

(defonce app_ (atom nil))

(defn -main [& _]
  (reset! app_
    (h/start-app
      {:max-refresh-ms 100
       :ctx-start      ctx-start
       :ctx-stop       ctx-stop
       :csrf-secret    (h/env :csrf-secret)})))

;; Refresh app when you re-eval file
(h/refresh-all!)

(comment
  (do (-main) nil)
  ;; (clojure.java.browse/browse-url "http://localhost:8080/")
  

  ;; stop server
  ((@app_ :stop))

  (def db (-> @app_ :ctx :db))

  ,)

(comment
  (def db (-> @app_ :ctx :db))
  (d/pragma-check db)
  
  ;; Execution time mean : 148.516131 ms
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
  
  ;; Execution time mean : 151.256280 µs
  (user/bench (do (UserView {:x 1 :y 1} db) nil))

  (d/table-info db :chunks)
  (d/table-list db)

  ;; (+ 7784 3249)

  ,)

(comment
  (user/bench
    (d/q db
      ["SELECT chunk_id, JSON_GROUP_ARRAY(state) AS chunk_cells FROM cell WHERE chunk_id IN (?, ?, ?, ?, ?, ?, ?, ?, ?)  GROUP BY chunk_id" 1978 3955 5932 1979 3956 5933 1980 3957 5934]))

  (def tab-state (-> @app_ :ctx :tab))

  (count @tab-state)

  (def db-write (-> @app_ :ctx :db-write))

  (d/q db-write
    {:update :chunk
     :set    {:chunk [:lift blank-chunk]}
     :where  [:= :id 0]})

  ;; Free up space (slow)
  ;; (time (d/q db-write ["VACUUM"]))
  ;; Checkpoint the WAL
  (d/q db-write ["PRAGMA wal_checkpoint(PASSIVE)"])
  (d/q db-write ["PRAGMA wal_checkpoint(TRUNCATE)"])

  

  ,)

(comment ;; Profiling
  (prof/start)
  (prof/stop)
  (prof/serve-ui 7777)
  ;; (clojure.java.browse/browse-url "http://localhost:7777/")
  )

(comment
  (def wrapped-router (-> @app_ :wrapped-router))
  
  (future
    (time
      (run!
        (fn [_]
          (run!
            (fn [_]
              (wrapped-router
                {:headers
                 {"accept-encoding" "br"
                  "cookie"          "__Host-sid=5SNfeDa90PhXl0expOLFGdjtrpY; __Host-csrf=3UsG62ic9wLsg9EVQhGupw"
                  "content-type"    "application/json"}
                 :request-method :post
                 :uri            handler-check
                 :body           {:csrf     "3UsG62ic9wLsg9EVQhGupw"
                                  :parentid "0"
                                  :targetid (str (rand-int 200))}}))
            ;; 10000r/s
            (range 10))
          (Thread/sleep 1))
        (range 10000))))

  
  )
