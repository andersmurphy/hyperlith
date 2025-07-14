(ns app.main
  (:gen-class)
  (:require [hyperlith.core :as h :refer [defaction defview]]
            [hyperlith.extras.sqlite :as d]
            [clojure.math :as math]))

(def chunk-size 16)
(def board-size (->> (math/pow chunk-size 2)
                  (/ 1000000000)
                  math/sqrt
                  math/ceil
                  int))
(def board-size-px (* 32 chunk-size board-size))
(def size (* board-size chunk-size))

(def css
  (let [black         "#000000"
        white         "#FFF1E8"
        accent        "#008751"
        other         "#FF004D"
        board-size-px (str board-size-px "px")]
    (h/static-css
      [["*, *::before, *::after"
        {:box-sizing :border-box
         :margin     0
         :padding    0}]

       [:html
        {:font-family "Arial, Helvetica, sans-serif"
         :font-size   :1.0rem
         :color       black
         :background  white}]

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
         :border                (str "1px solid " black)
         :grid-template-rows    (str "repeat(" board-size ", 1fr)")
         :grid-template-columns (str "repeat(" board-size ", 1fr)")
         :pointer-events        :none}]

       [:.chunk
        {:background            white
         :display               :grid
         :grid-template-rows    (str "repeat(" chunk-size ", 1fr)")
         :grid-template-columns (str "repeat(" chunk-size ", 1fr)")}]

       [:.pop
        {;; Animation that depresses the element
         :transform      "scale(0.8)"
         :transition     "scale 0.6s ease"
         ;; Disable element until next frame/morph
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

       [:.cell
        {:font-size      :1.2rem
         :background     white
         :border         (str "1px solid " black)
         :display        :grid
         :overflow       :hidden
         :white-space    :nowrap
         :place-content  :center
         :pointer-events :all}]

       [:.focus-cell
        {:background white
         :position   :relative
         :border     (str "1px solid " black)
         :font-size  :1.2rem}]

       [:.focus-user
        {:font-size      :1.2rem
         :background     white
         :height         :100%
         :min-width      :100%
         :overflow-x     :visible
         :white-space    :nowrap
         :position       :absolute
         :field-sizing   :content
         :outline        :none
         :border         (str "4px solid " accent)
         :pointer-events :all}]

       [:.focus-other
        {:font-size      :1.2rem
         :background     white
         :height         :100%
         :min-width      :100%
         :overflow-x     :visible
         :white-space    :nowrap
         :position       :absolute
         :outline        :none
         :border         (str "4px solid " other)
         :pointer-events :none}]

       [:.jump-input
        {:background    white
         :width         :6rem
         :font-size     :1.2rem
         :border-radius :0.15em
         :border        "0.15em solid currentColor"
         :padding       :5px}]])))
(defn get-tab-state [db sid tabid]
  (-> (d/q db ["SELECT state FROM tab WHERE (sid = ?) AND (tabid = ?)"
               sid tabid])
    first))

(defn update-tab-state! [db sid tabid update-fn]
  (let [old-state (get-tab-state db sid tabid)
        new-state (update-fn old-state)]
    (if old-state
      (d/q db
        ["UPDATE tab SET state = ? WHERE (sid = ?) AND (tabid = ?)"
         new-state sid tabid])
      (d/q db
        ["INSERT INTO tab (sid, tabid, state) VALUES (?, ?, ?)"
         sid
         tabid
         (assoc new-state
           :sid sid
           :tabid tabid)]))))

(defn update-chunk! [db chunk-cache chunk-id update-fn]
  (let [old-chunk (or (@chunk-cache chunk-id)
                    (-> (d/q db
                          ["SELECT chunk FROM chunk WHERE id = ?" chunk-id])
                      first))
        new-chunk (update-fn old-chunk)]
    (swap! chunk-cache assoc chunk-id new-chunk)))

(defaction handler-scroll
  [{:keys [sid tabid tx-batch!] {:keys [x y]} :body}]
  (tx-batch!
    (fn [db _]
      (update-tab-state! db sid tabid
        #(assoc %
           :x (max (int x) 0)
           :y (max (int y) 0))))))

(defn remove-focus! [sid tabid db chunk-cache]
  (let [{:keys [focus-chunk-id focus-cell-id]}
        (get-tab-state db sid tabid)]
    (when (and focus-chunk-id focus-cell-id)
      (update-chunk! db chunk-cache focus-chunk-id
        ;; Should this be tab id too?
        #(update % focus-cell-id dissoc :focus)))))

(defaction handler-focused
  [{:keys                       [sid tx-batch! tabid]
    {:keys [targetid parentid]} :body}]
  (when (and targetid parentid)
    (let [cell-id  (int (parse-long targetid))
          chunk-id (int (parse-long parentid))]
      (when (>= (dec (* chunk-size chunk-size)) cell-id 0)
        (tx-batch! (partial remove-focus! sid tabid))
        (tx-batch!
          (fn [db chunk-cache]
            (update-chunk! db chunk-cache chunk-id
              ;; Should this be tab id too?
              #(assoc-in % [cell-id :focus] sid))
            (update-tab-state! db sid tabid
              #(assoc % :focus-chunk-id chunk-id
                 :focus-cell-id cell-id))))))))

(defaction handler-save-cell
  [{:keys                                 [tx-batch!]
    {:keys [targetid parentid cellvalue]} :body}]
  (when (and targetid parentid)
    (let [cell-id  (int (parse-long targetid))
          chunk-id (int (parse-long parentid))]
      (when (>= (dec (* chunk-size chunk-size)) cell-id 0)
        (tx-batch!
          (fn [db chunk-cache]
            (update-chunk! db chunk-cache chunk-id
              #(assoc-in % [cell-id :value]
                 (subs cellvalue 0 (min (count cellvalue) 20))))))))))

(defn Cell [chunk-id local-id {:keys [value focus]} sid]
  (cond
    (and focus (= focus sid))
    (let [on-load  (str "$cellvalue = '" (or value "") "';el.focus();")
          on-input (str "@post('" handler-save-cell "')")
          id       (str "focus-" local-id)]
      (h/html
        [:div.focus-cell
         [:input.focus-user
          (array-map
            :id                            id
            :data-id                       local-id
            :data-parentid                 chunk-id
            :maxlength                     20
            :size                          10
            :type                          "text"
            :data-on-load                  on-load
            :data-preserve-attr            "data-on-load"
            :data-bind                     "cellvalue"
            :data-on-input__debounce.200ms on-input)]]))

    focus
    (h/html
      [:div.focus-cell
       [:p.focus-other
        {:data-id       local-id
         :data-parentid chunk-id
         :data-action   handler-focused}
        value]])

    :else (h/html [:p.cell
                   {:data-id       local-id
                    :data-parentid chunk-id
                    :data-value    value
                    :data-action   handler-focused}
                   value])))

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

(defn Chunk [chunk-id chunk-cells sid]
  (let [[x y] (chunk-id->xy chunk-id)
        x     (inc x)
        y     (inc y)]
    (h/html
      [:div.chunk
       {:id      chunk-id
        :data-id chunk-id
        :style   {:grid-column x :grid-row y}}
       (into []
         (map-indexed (fn [local-id box]
                        (Cell chunk-id local-id box sid)))
         chunk-cells)])))

(defn UserView [{:keys [x y sid] :or {x 0 y 0}} db]
  (->> (d/q db
         (into ["SELECT id, chunk FROM chunk WHERE id IN (?, ?, ?, ?, ?, ?, ?, ?, ?)"]
           (xy->chunk-ids x y)))
    (mapv (fn [[id chunk]]
            (Chunk id chunk sid)))))

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

(def shim-headers
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href css}]
    [:title nil "One billion cells"]
    [:meta {:content "So many cells" :name "description"}]))

(defview handler-root
  {:path     "/" :shim-headers shim-headers :br-window-size 19
   :on-close (fn [{:keys [tx-batch! sid tabid]}]
               (tx-batch! (partial remove-focus! sid tabid)))}
  [{:keys [db sid tabid] :as _req}]
  (let [user    (get-tab-state db sid tabid)
        content (UserView user db)]
    (h/html
      [:link#css {:rel "stylesheet" :type "text/css" :href css}]
      [:main#morph.main
       {:data-on-mousedown
        (str
          "if (evt.target.dataset.action) {"
          "evt.target.classList.add('pop');"
          "$targetid = evt.target.dataset.id;"
          "$parentid = evt.target.dataset.parentid;"
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
       [:h1 "One Billion Cells"]
       [:p "Built using "
        [:a {:href "https://clojure.org/"} "Clojure"]
        " and "
        [:a {:href "https://data-star.dev"} "Datastar"]
        " - "
        [:a {:href "https://github.com/andersmurphy/hyperlith/blob/master/examples/billion_cells/src/app/main.clj" } "source"]
        " - "
        [:a {:href "https://checkboxes.andersmurphy.com"} " more like this"]]])))

(def blank-chunk
  (-> (repeat (* chunk-size chunk-size) {})
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
                                  ["UPDATE chunk SET chunk = ? WHERE id = ?"
                                   new-chunk chunk-id]))
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

  

  
  (d/q db {:select [[[:count [:distinct :sid]]]]
           :from   :tab})
  ;; 4677
  

  ,)

;; focus element
;; focus element value
;; swap
