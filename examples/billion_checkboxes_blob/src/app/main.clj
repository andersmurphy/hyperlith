(ns app.main
  (:gen-class)
  (:require
   [app.qrcode :as qrcode]
   [clj-async-profiler.core :as prof]
   [clojure.math :as math]
   [clojure.pprint :as pprint]
   [hyperlith.core :as h :refer [defaction defview]]
   [hyperlith.extras.ui.virtual-scroll :as vs]
   [hyperlith.impl.sqlite :as d])
  (:import
   [java.util Arrays]))

(set! *warn-on-reflection* true)
;; (set! *unchecked-math* :warn-on-boxed)

(def cell-size-px 32)
(def chunk-size 16)
(def chunk-size-px (* cell-size-px chunk-size))
(def board-size (->> (math/pow chunk-size 2)
                  (/ 1000000000)
                  math/sqrt
                  math/ceil
                  int))
(def board-size-px (* cell-size-px chunk-size board-size))
(def size (* board-size chunk-size))
(def states
  [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14])

(def palette
  [0xFF004D
   0x29ADFF
   0x00E436
   0xFFA300
   0xFF77A8
   0x7E2553
   0xFFCCAA
   0x1D2B53
   0xAB5236
   0xFFEC27
   0x008751
   0xC2C3C7
   0x83769C
   0x5F574F])

(def icon
  (h/static-asset
    {:body         (h/load-resource "check.png")
     :content-type "image/png"}))

(def black "#000000")
(def white "#FFF1E8")

(def css
  (let [black  "#000000"
        white  "#FFF1E8"
        accent "#FFA300"]
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

       [:.pe-none
        {:pointer-events :none
         :user-select    :none}]

       ["@keyframes pop"
        "{  0% {transform: scale(1);}
           25% {transform: scale(0.8);}
          100% {transform: scale(1);}}"]

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
         :max-height     :100dvh
         :width          :100dvw
         :max-width      :100dvw
         :padding-inline :2dvw
         :padding-block  :2dvh
         :gap            :5px
         :display        :flex
         :flex-direction :column}]

       [:.view-wrapper
        {:min-height (str cell-size-px "px")
         :min-width  (str cell-size-px "px")}]

       [:.controls-wrapper
        {:gap            :5px
         :display        :flex
         :flex-direction :column}]

       [:.chunk
        {:background               white
         :display                  :grid
         :grid-template-rows
         (str "repeat("chunk-size","cell-size-px"px)")
         :grid-template-columns
         (str "repeat("chunk-size","cell-size-px"px)")
         :grid-column              (str "span " chunk-size)
         :grid-row                 (str "span " chunk-size)
         :contain                  :strict
         :contain-intrinsic-height (str (* chunk-size cell-size-px)"px")
         :contain-intrinsic-width  (str (* chunk-size cell-size-px)"px")}]

       (let [padding 5]
         [:.box
          {:width          (str (- cell-size-px (* 2 padding)) "px")
           :height         (str (- cell-size-px (* 2 padding)) "px")
           :font           :inherit
           :font-size      :1.2rem
           :padding        (str padding "px")
           :color          :currentColor
           :border         "0.15em solid currentColor"
           :border-radius  :0.15em
           :display        :grid
           :place-content  :center
           :pointer-events :all}])

       (map-indexed
         (fn [i x]
           [(str " :is(.box, .palette-item)[data-color='" (inc i) "']")
            {:background-color
             (format "#%06X" x)}])
         palette)

       [".box[data-color]:not([data-color='0'])::before"
        {:content    "\"\""
         :width      "0.50em"
         :height     "0.50em"
         :clip-path  "polygon(14% 44%, 0 65%, 50% 100%, 100% 16%, 80% 0%, 43% 62%)"
         :box-shadow (str "inset 1em 1em " white)}]

       [:.pop
        {;; Animation that depresses the element
         :animation      "pop .3s ease"
         ;; Disable element until this class is removed
         :pointer-events :none}]

       [:.palette
        {:margin-block          :5px
         :font-size             :1.2rem
         :background            white
         :width                 "min(100% - 2rem , 62rem)"
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
         :flex-wrap      :wrap
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
         :padding       :5px}]

       [:.button
        {:background    white
         :font-size     :1.2rem
         :border-radius :0.15em
         :border        "0.15em solid currentColor"
         :padding       :5px}]

       [:.toast
        {:backdrop-filter "blur(5px)"
         :position        :absolute
         :top             0
         :left            0
         :width           :100%
         :height          :100%
         :display         :grid
         :place-items     :center
         :text-align      :center
         :z-index         10}]

       [:.toast-card
        {:animation      "pop .3s ease"
         :pointer-events :none
         :background     white
         :font-size      :1.2rem
         :border-radius  :0.15em
         :border         "0.15em solid currentColor"
         :padding        :10px}]

       [:.qrcode
        {:border-radius :15px
         :padding       :8px
         :margin-top    :8px}]])))

(defn get-session-data [db sid]
  (-> (d/q db '{select [data]
                from   session
                where  [= id ?sid]
                limit  1}
        {:sid sid})
    first
    h/json->edn))

(defn get-tab-data [db sid tabid]
  (-> (get-session-data db sid) :tabs (get (keyword tabid))))

(defn update-tab-data! [db sid tabid update-fn]
  (let [tabid    (keyword tabid)
        old-data (get-session-data db sid)
        new-data (update-in old-data [:tabs tabid] update-fn)]
    (if old-data
      (d/q db '{update session
                set    {data ?new-data}
                where  [= id ?sid]}
        {:sid sid :new-data (h/edn->json new-data)})
      (d/q db '{insert-into session
                values      [{:id   ?sid
                              :data ?new-data}]}
        {:sid sid :new-data (h/edn->json new-data)}))))

(def ^byte/1 blank-chunk
  (byte-array (* chunk-size chunk-size)))

(defaction handler-scroll
  [{:keys [sid tabid ::h/tx!] {:keys [view-x view-y]} :body}]
  (tx!
    (fn [db _]
      (update-tab-data! db sid tabid
        #(assoc %
           :x (max (int view-x) 0)
           :y (max (int view-y) 0))))))

(defaction handler-resize
  [{:keys [sid tabid ::h/tx!] {:keys [view-h view-w]} :body}]
  (when (and view-h view-w)
    (tx!
      (fn [db _]
        (update-tab-data! db sid tabid
          #(assoc %
             :height (max (int view-h) 0)
             :width  (max (int view-w) 0)))))))

(defaction handler-palette
  [{:keys [sid tabid ::h/tx!] {:keys [targetid]} :body}]
  (let [color (parse-long targetid)]
    ;; 0 is an empty color (used for clearing)
    (when (<= 0 color (dec (count states)))
      (tx!
        (fn [db _]
          (update-tab-data! db sid tabid #(assoc % :color color)))))))

(defaction handler-check
  [{:keys                       [sid tabid ::h/tx!]
    {:keys [targetid parentid]} :body}]
  (when (and targetid parentid)
    (let [cell-id  (int (parse-long targetid))
          chunk-id (int (parse-long parentid))]
      (when (and
              (>= (dec (* chunk-size chunk-size)) cell-id  0)
              (>= (dec (* board-size board-size)) chunk-id 0))
        (tx!
          (fn [db chunk-cache]
            (let [user-color (or (:color (get-tab-data db sid tabid)) 1)
                  chunk      (or (@chunk-cache chunk-id)
                               (-> (d/q db '{select [data]
                                             from   chunk
                                             where  [= id ?chunk-id]}
                                     {:chunk-id chunk-id})
                                 first)
                               (do
                                 (d/q db
                                   '{insert-into chunk
                                     values      [{id   ?chunk-id
                                                   data ?blank-chunk}]}
                                   {:chunk-id    chunk-id
                                    :blank-chunk blank-chunk})
                                 (d/q db
                                   '{insert-into chunk-html
                                     values      [{chunk-id ?chunk-id}]}
                                   {:chunk-id chunk-id})
                                 (java.util.Arrays/copyOf blank-chunk
                                   (alength blank-chunk))))]
              (swap! chunk-cache assoc chunk-id
                (do (aset-byte chunk cell-id
                      (if (= (byte 0) (aget (bytes chunk) cell-id))
                        (byte user-color)
                        (byte 0)))
                    chunk)))))))))

(defn scroll-to-xy-js [x y]
  (str
    "$_view.scroll(" (int (* (/ x size) board-size-px))
    "," (int (* (/ y size) board-size-px)) ");"))

(defaction handler-jump
  [{:keys [sid tabid ::h/tx!] {:keys [jumpx jumpy]} :body}]
  (tx!
    (fn [db _]
      (update-tab-data! db sid tabid
        #(assoc % :jump-x jumpx :jump-y jumpy :jump-id (h/new-uid))))))

(defaction handler-share
  [{:keys [sid tabid ::h/tx!] {:keys [jumpx jumpy]} :body}]
  (tx!
    (fn [db _]
      (update-tab-data! db sid tabid
        #(assoc % :share-x jumpx :share-y jumpy :share-id (h/new-uid))))))

(defn Checkbox [local-id state]
  (h/html
    [:div.box
     {:data-color state
      :data-id    local-id}]))

(defn xy->chunk-id [x y]
  (+ x (* y board-size)))

(defn xy->chunk-ids
  [{:keys [x-offset-items y-offset-items x-rendered-items y-rendered-items]}]
  (-> (for [y (range y-offset-items (+ y-offset-items y-rendered-items))
            x (range x-offset-items (+ x-offset-items x-rendered-items))]
        (xy->chunk-id x y))
    vec))

(defn Chunk [chunk-id chunk-cells]
  (h/html
    [:div.chunk
     {:id          (str "chunk-" chunk-id)
      :data-ignore true
      :data-id     chunk-id
      :data-action handler-check}
     (into []
       (map-indexed (fn [local-id box] (Checkbox local-id box)))
       chunk-cells)]))

(def empty-checks
  (h/html
    (into []
      (map-indexed (fn [local-id box] (Checkbox local-id box)))
      blank-chunk)))

(defn EmptyChunk [chunk-id]
  (-> (h/html
        [:div.chunk
         {:id                (str "chunk-" chunk-id)
          :data-ignore-morph true
          :data-ignore       true
          :data-id           chunk-id}
         empty-checks])
    h/html->str))

(defn UserView
  [db offset-data]
  {:content (->> (xy->chunk-ids offset-data)
              (mapv (fn [chunk-id]
                      (let [[[id html]]
                            (d/q db
                              '{select [chunk-html.chunk-id chunk-html.data]
                                from   chunk
                                join   [:chunk-html [= chunk-id chunk.id]]
                                where  [= chunk.id ?chunk-id]}
                              {:chunk-id chunk-id})]
                        (-> (if id html (EmptyChunk chunk-id))
                          h/html-raw-str)))))})

(def copy-xy-to-clipboard-js "navigator.clipboard.writeText(`https://checkboxes.andersmurphy.com?x=${$jumpx}&y=${$jumpy}`)")

(defn Palette [current-selected]
  (h/html
    [:div.palette nil
     (mapv (fn [state]
             (h/html [:div.palette-item
                      {:data-id     state
                       :data-action handler-palette
                       :data-color  state
                       :class       (when (= current-selected state)
                                      "palette-selected")}]))
       (subvec states 1))]))

(def shim-headers
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href css}]
    [:title nil "One billion checkboxes"]
    [:link {:rel "icon" :type "image/png" :href icon}]
    [:meta {:content "So many checkboxes" :name "description"}]))

(defn scroll->cell-xy-js [n]
  (str "Math.round((" n "/" board-size-px ")*" size ")"))

(defview handler-root
  {:path              "/" :shim-headers shim-headers :br-window-size 24
   :render-on-connect false
   :on-open           (fn [{:keys [::h/tx!]}]
                        ;; This will trigger a batch on new user connect
                        ;; But not actually update the database
                        (tx! (fn [& _] nil)))}
  [{:keys         [db sid tabid]
    {:strs [x y]} :query-params
    :as           _req}]
  (let [init-jump-x                                     (h/try-parse-long x 0)
        init-jump-y                                     (h/try-parse-long y 0)
        tab-data                                        (get-tab-data db sid tabid)
        {:keys [x y height width share-id
                share-x share-y jump-x jump-y jump-id]} tab-data
        palette                                         (Palette (or (:color tab-data) 1))]
    [(h/html [:link#css {:rel "stylesheet" :type "text/css" :href css}])
     (h/html
       [:main#morph.main
        {:data-on:mousedown
         (str
           "if (evt.target.dataset.action || evt.target.parentElement.dataset.action) {"
           "evt.target.classList.add('pop');"
           "$targetid = evt.target.dataset.id;"
           "$parentid = evt.target.parentElement.dataset.id;"
           "@post(`${evt.target.dataset.action || evt.target.parentElement.dataset.action}`);"
           "setTimeout(() => evt.target.classList.remove('pop'), 300)"
           "}")}
        [:div.view-wrapper
         [::vs/virtual#view
          {:data-ref              "_view"
           :v/x                   {:item-size          chunk-size-px
                                   :buffer-items       1
                                   :max-rendered-items 5
                                   :scroll-pos         x
                                   :view-size          width
                                   :item-count-fn      (fn [] board-size)
                                   :chunk-size         chunk-size}
           :v/y                   {:item-size          chunk-size-px
                                   :buffer-items       1
                                   :max-rendered-items 5
                                   :scroll-pos         y
                                   :view-size          height
                                   :item-count-fn      (fn [] board-size)
                                   :chunk-size         chunk-size}
           :v/item-fn             (partial UserView db)
           :v/scroll-handler-path handler-scroll
           :v/resize-handler-path handler-resize}]]
        [:div.controls-wrapper
         {;; firefox sometimes preserves scroll on refresh and we don't want that
          :data-init (scroll-to-xy-js init-jump-x init-jump-y)}
         [:div.jump
          [:h2 "X:"]
          [:input.jump-input
           {:type "number" :data-bind "jumpx"
            :data-effect
            (str  "$view-x;@peek(() => {$jumpx = "(scroll->cell-xy-js "$view-x")"})")}]
          [:h2 "Y:"]
          [:input.jump-input
           {:type "number" :data-bind "jumpy"
            :data-effect
            (str  "$view-y;@peek(() => {$jumpy = "(scroll->cell-xy-js "$view-y")"})")}]
          [:div.button {:data-action handler-jump}
           [:strong.pe-none "JUMP"]]
          [:div.button {:data-action       handler-share
                        :data-on:mousedown copy-xy-to-clipboard-js}
           [:strong.pe-none "SHARE"]]]
         palette
         [:h1 "One Billion Checkboxes"]
         [:p "Built using "
          [:a {:href "https://clojure.org/"} "Clojure"]
          " and "
          [:a {:href "https://data-star.dev"} "Datastar"]
          " - "
          [:a {:href "https://github.com/andersmurphy/hyperlith/blob/master/examples/billion_checkboxes_blob/src/app/main.clj" } "source"]
          " - "
          [:a {:href "https://andersmurphy.com/about"} "blog"]]]
        (when share-id
          [:div {:id share-id :data-ignore-morph true}
           [:div.toast {:data-on:mousedown "el.remove()"}
            [:div.toast-card
             [:p [:strong nil (str "X: " share-x " Y: " share-y)]]
             [:p [:strong "SHARE URL COPIED TO CLIPBOARD"]]
             [:div.qrcode nil
              (qrcode/url->qrcode-svg
                (str "https://checkboxes.andersmurphy.com?x="
                  share-x "&y=" share-y)
                {:dark black :light white})]]]])
        (when jump-id
          (h/execute-expr jump-id (scroll-to-xy-js jump-x jump-y)))])]))

(defn migrations [db]
  ;; Note: all this code must be idempotent
  ;; Create tables
  (println "Running migrations...")
  (d/q db
    ["CREATE TABLE IF NOT EXISTS chunk(id INTEGER PRIMARY KEY, data BLOB)"])
  (d/q db
    ["CREATE TABLE IF NOT EXISTS chunk_html(chunk_id INTEGER PRIMARY KEY, data BLOB, FOREIGN KEY(chunk_id) REFERENCES chunk(id))"])
  (d/q db
    ["CREATE TABLE IF NOT EXISTS session(id TEXT PRIMARY KEY, data TEXT) WITHOUT ROWID"]))

(defn batch-fn [{:keys [db]} thunks]
  (let [chunk-cache (atom {})]
    (d/with-write-tx [db db]
      (run! (fn [thunk] (thunk db chunk-cache)) thunks)
      (run! (fn [[chunk-id new-chunk]]
              (d/q db '{update chunk
                        set    {id ?chunk-id data ?new-chunk}
                        where  [= id ?chunk-id]}
                {:chunk-id chunk-id :new-chunk new-chunk})
              (d/q db '{update chunk_html
                        set    {chunk-id ?chunk-id
                                data     ?new-html}
                        where  [= chunk-id ?chunk-id]}
                {:chunk-id  chunk-id
                 :new-chunk new-chunk
                 :new-html  (-> (Chunk chunk-id new-chunk)
                              h/html->str)}))
        @chunk-cache))))

(defonce app_ (atom nil))

(defn start-app! [& {:keys [dev?]}]
  (reset! app_
    (h/start-app
      {:dbs
       {:db {:name          "database-new.db"
             :pragma-writer {:cache_size 8000}
             :pragma
             {:journal_mode "TRUNCATE"
              :cache_size   2000
              :page_size    (* 4096 4)
              :mmap_size    268435456}}}
       :batch-fn      #'batch-fn
       :batch-tick-ms 100
       :email         (h/env :email)
       :domain        (h/env :domain)
       :dev?          dev?}))
  (let [{{:keys [::h/tx!]} :ctx} @app_]
    (tx! (fn [db _] (migrations db)
           (d/escape-write-tx [db db]
             (d/q db ["PRAGMA wal_checkpoint(TRUNCATE)"])
             (d/q db ["VACUUM"]))))))

(defn -main [& _]
  (start-app!))

(comment
  (do (start-app! :dev? true) nil)
  ;; (clojure.java.browse/browse-url "http://localhost:8080/")
  ;; stop server
  ((@app_ :stop!))

  ,)

(comment ;; Profiling

  (prof/start {:event :alloc})
  (prof/start)
  (prof/stop)
  (prof/serve-ui 7777)
  ;; (clojure.java.browse/browse-url "http://localhost:7777/")
  )

(comment ;; Example projection generation
  (def tx! (-> @app_ :ctx ::h/tx!))

  (tx!
    (fn [db _]
      (-> (d/q db ["pragma journal_mode"])
        pprint/pprint)))

  (tx!
    (fn [db _]
      (-> (d/q db ["pragma page_size"])
        pprint/pprint)))

  (tx!
    (fn [db _]
      (-> (d/q db '{select * from chunk where [= id 0]})
        pprint/pprint)))

  (tx!
    (fn [db _]
      (d/q db ["DROP TABLE chunk_html"])
      (d/q db
        ["CREATE TABLE IF NOT EXISTS chunk_html(chunk_id INTEGER PRIMARY KEY, data TEXT, FOREIGN KEY(chunk_id) REFERENCES chunk(id))"])
      (run!
        (fn [[id chunk]]
          (d/q db
            '{insert-into chunk-html
              values      [{chunk-id ?chunk-id data ?data}]}
            {:chunk-id id
             :data     (-> (Chunk id chunk)
                         h/html->str)}))
        (d/q db '{select * from chunk}))
      (-> (d/q db '{select [[[count *]]] from chunk-html})
        pprint/pprint))))

(comment ;; Example migration of for changing column type

  (tx!
    (fn [db _]
      (d/q db
        ["CREATE TABLE IF NOT EXISTS newchunk(id INTEGER PRIMARY KEY, data BLOB)"])
      (d/q db ["INSERT INTO newchunk SELECT * FROM chunk"])
      (d/q db ["DROP TABLE chunk"])
      (d/q db ["ALTER TABLE newchunk RENAME TO chunk"])
      (-> (d/q db '{select [[[count *]]] from chunk})
        pprint/pprint))))

(comment ;; clearing a chunk
  (def tx! (-> @app_ :ctx ::h/tx!))

  (tx!
    (fn [db _]
      (d/q db
        '{update chunk
          set    {data ?blank-chunk}
          where  [= id 0]}
        {:blank-chunk blank-chunk}))),)

;; migrate tab data
;; drop leading byte

(comment
  (def tx! (-> @app_ :ctx ::h/tx!))

  (tx!
    (fn [db _]
      (pprint/pprint
        (h/json->edn
          (first (d/q db '{select data from session})))))))
