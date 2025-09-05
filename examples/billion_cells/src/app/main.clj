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
(def cell-size 32)
(def board-size-px (* cell-size chunk-size board-size))
(def size (* board-size chunk-size))
(def black "#000000")
(def white "#FFF1E8")

(def css
  (let [accent        "#008751"
        other         "#FF004D"
        board-size-px (str board-size-px "px")
        max-width (* 20 cell-size)]
    (h/static-css
      [["*, *::before, *::after"
        {:box-sizing :border-box
         :margin     0
         :padding    0}]

       [:.toast
        {:animation      "pop .3s ease"
         :position       :absolute
         :pointer-events :none
         :top            0
         :left           0
         :width          :100%
         :height         :100%
         :display        :grid
         :place-items    :center
         :text-align     :center
         :z-index        10}]

       [:.pe-none
        {:pointer-events :none
         :user-select    :none}]

       ["@keyframes pop"
        "{  0% {transform: scale(1);}
           25% {transform: scale(0.8);}
          100% {transform: scale(1);}}"]

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
         :width          (str "min(100% - 2rem ," max-width "px)")
         :gap            :5px
         :flex-direction :column}]

       [:.view
        {:position        :relative
         :overflow        :scroll
         :scroll-behavior :smooth
         :scrollbar-color (str black " transparent")
         :overflow-anchor :none
         :width           (str "min(100% - 2rem ," max-width "px)")}]

       [:.board-container
        {:width                 board-size-px
         :height                board-size-px
         :position :relative}]

       [:.board
        {:position              :absolute
         :width                 board-size-px
         :height                board-size-px
         :display               :grid
         :grid-template-rows    (str "repeat(" board-size ", 1fr)")
         :grid-template-columns (str "repeat(" board-size ", 1fr)")
         :pointer-events        :none}]

       [:.board-background
        {:width        board-size-px
         :height       board-size-px}]

       [:.chunk
        {:display               :grid
         :grid-template-rows    (str "repeat(" chunk-size ", 1fr)")
         :grid-template-columns (str "repeat(" chunk-size ", 1fr)")}]

       [:.pop
        {;; Animation that depresses the element
         :animation      "pop .3s ease"
         ;; Disable element until this class is removed
         :pointer-events :none}]

       [:.jump
        {:display        :flex
         :gap            :5px
         :flex-direction :row
         :flex-wrap      :wrap
         :align-items    :center}]

       [:.jump-input
        {:background    white
         :width         :6rem
         :font-size     :1.2rem
         :border-radius :0.15em
         :border        "0.15em solid currentColor"
         :padding       :5px}]

       ["input[type=\"number\"]:focus"
        {:outline       :none
         :border-radius :0.15em
         :border        (str "0.15em solid " accent)}]

       [:.button
        {:background     white
         :font-size      :1.2rem
         :border-radius  :0.15em
         :border         "0.15em solid currentColor"
         :padding        :5px}]

       [:a {:color accent}]

       [:.cell
        {:width  (str cell-size "px")
         :height (str cell-size "px")
         :font-size      :1.2rem
         :display        :grid
         :overflow       :hidden
         :white-space    :nowrap
         :place-content  :center
         :pointer-events :all}]

       [:.focus-cell
        {:background  white
         :margin-left :2px
         :margin-top  :2px
         :position    :relative
         :font-size   :1.2rem}]

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
         :pointer-events :none}]])))

(defn get-session-data [db sid]
  (-> (d/q db
        '{select [data]
          from   session
          where  [= id ?sid]}
        {:sid sid})
    first))

(defn get-tab-data [db sid tabid]
  (-> (get-session-data db sid) :tabs (get tabid)))

(defn update-tab-data! [db sid tabid update-fn]
  (let [old-data (get-session-data db sid)
        new-data (update-in old-data [:tabs tabid] update-fn)]
    (if old-data
      (d/q db '{update session
                set    {data ?new-data}
                where  [= id ?sid]}
        {:sid sid :new-data new-data})
      (d/q db '{insert-into session
                values      [{id   ?sid
                              data ?new-data}]}
        {:sid      sid
         :new-data (assoc new-data :sid sid)}))))

(defn update-chunk! [db chunk-cache chunk-id update-fn]
  (let [old-chunk (or (@chunk-cache chunk-id)
                    (-> (d/q db '{select [data]
                                  from   chunk
                                  where  [= id ?chunk-id]}
                          {:chunk-id chunk-id})
                      first))
        new-chunk (update-fn old-chunk)]
    (swap! chunk-cache assoc chunk-id new-chunk)))

(defaction handler-scroll
  [{:keys [sid tabid tx-batch!] {:keys [x y]} :body}]
  (tx-batch!
    (fn [db _]
      (update-tab-data! db sid tabid
        #(assoc %
           :x (max (int x) 0)
           :y (max (int y) 0))))))

(defn remove-focus! [sid tabid db chunk-cache]
  (let [{:keys [focus-chunk-id focus-cell-id]}
        (get-tab-data db sid tabid)]
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
            (update-tab-data! db sid tabid
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

(defn scroll-to-xy-js [x y]
  (str "$_view.scroll(" (int (* (/ x size) board-size-px))
    "," (int (* (/ y size) board-size-px)) ");"))

(defaction handler-jump
  [{:keys [_sid _tabid _tx-batch!] {:keys [jumpx jumpy]} :body}]
  (h/execute-expr (scroll-to-xy-js jumpx jumpy)))

(defaction handler-share
  [{:keys [_sid _tabid _tx-batch!] {:keys [jumpx jumpy]} :body}]
  (h/html
    [:div.toast {:data-on-load__delay.3s "el.remove()"}
     [:div.button
      [:p [:strong nil (str "X: " jumpx " Y: " jumpy)]]
      [:p [:strong "SHARE URL COPIED TO CLIPBOARD"]]]]))

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
  (->> (let [[a b c d e f g h i] (xy->chunk-ids x y)]
         (d/q db
           '{select [id data]
             from   chunk
             where  [in id ?chunk-ids]}
           {:chunk-ids [a b c d e f g h i]}))
    (mapv (fn [[id chunk]]
            (Chunk id chunk sid)))))

(defn scroll-offset-js [n]
  (str "Math.round((" n "/" board-size-px ")*" board-size "-1)"))

(defn scroll->cell-xy-js [n]
  (str "Math.round((" n "/" board-size-px ")*" size ")"))

(def on-scroll-js
  (str
    "$jumpx = " (scroll->cell-xy-js "el.scrollLeft") ";"
    "$jumpy = " (scroll->cell-xy-js "el.scrollTop") ";"
    "let x = " (scroll-offset-js "el.scrollLeft") ";"
    "let y = " (scroll-offset-js "el.scrollTop") ";"
    "let change = x !== $x || y !== $y;"
    "$x = x; $y = y;"
    "change && @post(`" handler-scroll "`)"))

(def copy-xy-to-clipboard-js "navigator.clipboard.writeText(`https://cells.andersmurphy.com?x=${$jumpx}&y=${$jumpy}`)")

(def shim-headers
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href css}]
    [:title nil "One billion cells"]
    [:meta {:content "So many cells" :name "description"}]))

(defview handler-root
  {:path     "/" :shim-headers shim-headers :br-window-size 19
   :on-close (fn [{:keys [tx-batch! sid tabid]}]
               (tx-batch! (partial remove-focus! sid tabid)))}
  [{:keys         [db sid tabid]
    {:strs [x y]} :query-params
    :as           _req}]
  (let [x       (h/try-parse-long x 0)
        y       (h/try-parse-long y 0)
        user    (assoc (get-tab-data db sid tabid)
                  :sid sid :tabid tabid)
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
          "setTimeout(() => evt.target.classList.remove('pop'), 300)"
          "}")}
       [:div#view.view
        {;; firefox sometimes preserves scroll on refresh and we don't want that
         :data-on-load                                   (scroll-to-xy-js x y)
         :data-ref                                       "_view"
         :data-on-scroll__throttle.100ms.trail.noleading on-scroll-js}
        [:div.board-container
         [:div#board.board nil content]
         [:div.board-background nil
          [:svg {:width "100%" :height "100%"}
           [:defs
            [:pattern#grid
             {:width 32 :height 32 :patternUnits "userSpaceOnUse"}
             [:rect {:x            1  :y      1
                     :width        32 :height 32
                     :fill         "none"
                     :stroke       black
                     :stroke-width 2}]]]
           [:rect {:width "100%" :height "100%" :fill "url(#grid)"}]]]]]
       [:div.jump
        [:h2 "X:"] [:input.jump-input {:type "number" :data-bind "jumpx"}]
        [:h2 "Y:"] [:input.jump-input {:type "number" :data-bind "jumpy"}]
        [:div.button {:data-action handler-jump}
         [:strong.pe-none "GO"]]
        [:div.button
         {:data-action handler-share
          :data-on-mousedown copy-xy-to-clipboard-js}
         [:strong.pe-none "SHARE"]]]
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
                    '{insert-into chunk
                      values      [{id   ?chunk-id
                                    data ?blank-chunk}]}
                    {:chunk-id    (xy->chunk-id x y)
                     :blank-chunk blank-chunk}))
            board-range)
          (print ".") (flush))
        board-range)))
  nil)

(defn migrations [db]
  ;; Note: all this code must be idempotent
  ;; Create tables
  (println "Running migrations...")
  (d/q db
    ["CREATE TABLE IF NOT EXISTS chunk(id INT PRIMARY KEY, data BLOB)"])
  (d/q db
    ["CREATE TABLE IF NOT EXISTS session(id TEXT PRIMARY KEY, data BLOB) WITHOUT ROWID"])
  (when-not (d/q db '{select [id] from chunk limit 1})
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
                                (d/q db '{update chunk
                                          set    {data ?new-chunk}
                                          where  [= id ?chunk-id]}
                                  {:chunk-id  chunk-id
                                   :new-chunk new-chunk}))
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
      {:ctx-start      ctx-start
       :ctx-stop       ctx-stop
       :csrf-secret    (h/env :csrf-secret)})))

;; Refresh app when you re-eval file
(h/refresh-all!)

(comment
  (do (-main) nil)
  ;; (clojure.java.browse/browse-url "https://localhost:3030/")

  ;; stop server
  ((@app_ :stop))

  (def db (-> @app_ :ctx :db))


  (d/q db '{select [[[count *]]]from session})
  ;; 4897
  

  ,)
