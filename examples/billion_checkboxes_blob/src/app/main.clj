(ns app.main
  (:gen-class)
  (:require [app.qrcode :as qrcode]
            [hyperlith.core :as h :refer [defaction defview]]
            [hyperlith.extras.sqlite :as d]
            [hyperlith.extras.batch :as batch]
            [hyperlith.extras.ui.virtual-scroll :as vs]
            [clj-async-profiler.core :as prof]
            [clojure.math :as math]))

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
                values      [{:id   ?sid
                              :data ?new-data}]}
        {:sid sid :new-data new-data}))))

(def blank-chunk
  (byte-array (* chunk-size chunk-size)))

(defaction handler-scroll
  [{:keys [sid tabid tx-batch!] {:keys [view-x view-y]} :body}]
  (tx-batch!
    (fn [db _]
      (update-tab-data! db sid tabid
        #(assoc %
           :x (max (int view-x) 0)
           :y (max (int view-y) 0))))))

(defaction handler-resize
  [{:keys [sid tabid tx-batch!] {:keys [view-h view-w]} :body}]
  (when (and view-h view-w)
    (tx-batch!
      (fn [db _]
        (update-tab-data! db sid tabid
          #(assoc %
             :height (max (int view-h) 0)
             :width  (max (int view-w) 0)))))))

(defaction handler-palette
  [{:keys [sid tabid tx-batch!] {:keys [targetid]} :body}]
  (let [color (parse-long targetid)]
    ;; 0 is an empty color (used for clearing)
    (when (<= 0 color (dec (count states)))
      (tx-batch!
        (fn [db _]
          (update-tab-data! db sid tabid #(assoc % :color color)))))))

(defaction handler-check
  [{:keys                       [sid tx-batch! db tabid]
    {:keys [targetid parentid]} :body}]
  (when (and targetid parentid)
    (let [user-color (or (:color (get-tab-data db sid tabid)) 1)
          cell-id    (int (parse-long targetid))
          chunk-id   (int (parse-long parentid))]
      (when (>= (dec (* chunk-size chunk-size)) cell-id 0)
        (tx-batch!
          (fn [db chunk-cache]
            (let [chunk (or (@chunk-cache chunk-id)
                          (-> (d/q db '{select [data]
                                        from   chunk
                                        where  [= id ?chunk-id]}
                                {:chunk-id chunk-id})
                            first)
                          (d/q db
                            '{insert-into chunk
                              values      [{id   ?chunk-id
                                            data ?blank-chunk}]}
                            {:chunk-id    chunk-id
                             :blank-chunk blank-chunk})
                          (-> (d/q db '{select [data]
                                        from   chunk
                                        where  [= id ?chunk-id]}
                                {:chunk-id chunk-id})
                            first))]
              (swap! chunk-cache assoc chunk-id
                (do (aset-byte chunk cell-id
                      (if (= (byte 0) (aget ^byte/1 chunk cell-id))
                        (byte user-color)
                        (byte 0)))
                    chunk)))))))))

(defn scroll-to-xy-js [x y]
  (str
    "$_view.scroll(" (int (* (/ x size) board-size-px))
    "," (int (* (/ y size) board-size-px)) ");"))

(defaction handler-jump
  [{:keys [_sid _tabid _tx-batch!] {:keys [jumpx jumpy]} :body}]
  (h/execute-expr (scroll-to-xy-js jumpx jumpy)))

(defaction handler-share
  [{:keys [_sid _tabid _tx-batch!] {:keys [jumpx jumpy]} :body}]
  (h/html
    [:div.toast {:data-on:mousedown "el.remove()"}
     [:div.toast-card
      [:p [:strong nil (str "X: " jumpx " Y: " jumpy)]]
      [:p [:strong "SHARE URL COPIED TO CLIPBOARD"]]
      [:div.qrcode nil
       (qrcode/url->qrcode-svg
         (str "https://checkboxes.andersmurphy.com?x="
           jumpx "&y=" jumpy)
         {:dark black :light white})]]]))

(defn Checkbox [local-id state]
  (h/html
    [:div.box
     {:data-color  state
      :data-id     local-id
      :data-action handler-check}]))

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
      :data-id     chunk-id}
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
                      (let [[[id html]] (d/q db '{select [id html]
                                                  from   chunk
                                                  where  [= id ?chunk-id]}
                                          {:chunk-id chunk-id})]
                        (-> (if id
                              (String. ^byte/1 html)
                              (EmptyChunk chunk-id))
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
   :on-open           (fn [{:keys [tx-batch!]}]
                        ;; This will trigger a batch on new user connect
                        ;; But not actually update the database
                        (tx-batch! (fn [& _] nil)))}
  [{:keys         [db sid tabid]
    {:strs [x y]} :query-params
    :as           _req}]
  (let [jump-x                     (h/try-parse-long x 0)
        jump-y                     (h/try-parse-long y 0)
        tab-data                   (get-tab-data db sid tabid)
        {:keys [x y height width]} tab-data
        palette                    (Palette (or (:color tab-data) 1))]
    (h/html
      [:link#css {:rel "stylesheet" :type "text/css" :href css}]
      [:main#morph.main
       {:data-on:mousedown
        (str
          "if (evt.target.dataset.action) {"
          "evt.target.classList.add('pop');"
          "$targetid = evt.target.dataset.id;"
          "$parentid = evt.target.parentElement.dataset.id;"
          "@post(`${evt.target.dataset.action}`);"
          "setTimeout(() => evt.target.classList.remove('pop'), 300)"
          "}")}
       [:div.view-wrapper
        [::vs/virtual#view
         {:data-ref              "_view"
          :v/x                   {:item-size          chunk-size-px
                                  :buffer-items       2
                                  :max-rendered-items 7
                                  :scroll-pos         x
                                  :view-size          width
                                  :item-count-fn      (fn [] board-size)
                                  :chunk-size         chunk-size}
          :v/y                   {:item-size          chunk-size-px
                                  :buffer-items       2
                                  :max-rendered-items 7
                                  :scroll-pos         y
                                  :view-size          height
                                  :item-count-fn      (fn [] board-size)
                                  :chunk-size         chunk-size}
          :v/item-fn             (partial UserView db)
          :v/scroll-handler-path handler-scroll
          :v/resize-handler-path handler-resize}]]
       [:div.controls-wrapper
        {;; firefox sometimes preserves scroll on refresh and we don't want that
         :data-init (scroll-to-xy-js jump-x jump-y)}
        [:div.jump
         [:h2 "X:"]
         [:input.jump-input
          {:type        "number" :data-bind "jumpx"
           :data-effect
           (str  "$view-x;@peek(() => {$jumpx = "(scroll->cell-xy-js "$view-x")"})")}]
         [:h2 "Y:"]
         [:input.jump-input
          {:type        "number" :data-bind "jumpy"
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
         [:a {:href "https://andersmurphy.com/about"} "blog"]]]])))

(defn migrations [db]
  ;; Note: all this code must be idempotent
  ;; Create tables
  (println "Running migrations...")
  (d/q db
    ["CREATE TABLE IF NOT EXISTS chunk(id INTEGER PRIMARY KEY, data BLOB, html BLOB)"])
  (d/q db
    ["CREATE TABLE IF NOT EXISTS session(id TEXT PRIMARY KEY, data BLOB) WITHOUT ROWID"]))

(defn batch-fn [writer thunks]
  #_{:clj-kondo/ignore [:unresolved-symbol]}
  (let [chunk-cache (atom {})]
    (d/with-write-tx [db writer]
      (run! (fn [thunk] (thunk db chunk-cache)) thunks)
      (run! (fn [[chunk-id new-chunk]]
              (d/q db '{update chunk
                        set    {data ?new-chunk
                                html ?new-html}
                        where  [= id ?chunk-id]}
                {:chunk-id  chunk-id
                 :new-chunk new-chunk
                 :new-html (String/.getBytes
                             (h/html->str (Chunk chunk-id new-chunk)))}))
        @chunk-cache)))
  (h/refresh-all!))

(defn ctx-start []
  (let [db-name "database-new.db"
        _       (d/restore-then-replicate! db-name
                  {:s3-access-key-id     (h/env :s3-access-key-id)
                   :s3-access-secret-key (h/env :s3-access-secret-key)
                   :config-yml
                   (h/edn->json
                     {:dbs
                      [{:path db-name
                        :replicas
                        [{:type          "s3"
                          :bucket        "hyperlith"
                          :endpoint      "https://nbg1.your-objectstorage.com"
                          :region        "nbg1"
                          :sync-interval "1s"}]}]}
                     :escape-slash false)})
        {:keys [writer reader]}
        (d/init-db! db-name
          {:pool-size 4})]
    ;; Run migrations
    (migrations writer)
    {:db        reader
     :db-read   reader
     :db-write  writer
     :tx-batch! (batch/async-batcher-init! writer
                  {:batch-fn      #'batch-fn
                   :batch-tick-ms 50})}))

(defn ctx-stop [ctx]
  (.close (:db-write ctx))
  (.close (:db-read ctx)))

(defonce app_ (atom nil))

(defn -main [& _]
  (reset! app_
    (h/start-app
      {:ctx-start   ctx-start
       :ctx-stop    ctx-stop})))

;; Refresh app when you re-eval file
(h/refresh-all!)

(comment
  (do (-main) nil)
  ;; (clojure.java.browse/browse-url "https://localhost:3030/")

  ;; stop server
  ((@app_ :stop))

  (def db (-> @app_ :ctx :db))

  ,)

(comment
  (def db (-> @app_ :ctx :db))
  (d/pragma-check db)

  ;; Execution time mean : 78.177554 ms
  ;; Execution time mean : 34.990452 ms
  ;; Execution time mean : 25.271390 ms
  (user/bench
    (->> (mapv
           (fn [n]
             (future
               (let [n (mod n board-size)]
                 (UserView db {:x-offset-items   0 :y-offset-items   0
                               :x-rendered-items 7 :y-rendered-items 7})

                 ;; we don't want to hold onto the object
                 ;; not realistic
                 nil)))
           (range 0 100))
      (run! (fn [x] @x))))

  ;; Execution time mean : 2.424361 ms
  ;; Execution time mean : 1.159274 ms
  ;; Execution  time mean : 686.989437 µs
  (user/bench
    (do
      (UserView db {:x-offset-items   0 :y-offset-items   0
                    :x-rendered-items 7 :y-rendered-items 7}) nil))


  (d/table-info db :chunk)
  (d/table-list db)
  (d/q db '{select [[[count *]]] from session}) ;; 5624

  ;; (+ 17209)

  ,)

(comment

  (def tab-state (-> @app_ :ctx :tab))

  (count @tab-state)

  (def db-write (-> @app_ :ctx :db-write))

  (d/q db-write
    '{update chunk
      set    {data ?blank-chunk}
      where  [= id 0]}
    {:blank-chunk blank-chunk})

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
  (def db-write (-> @app_ :ctx :db-write))
  
  (user/bench
    (do (d/q db-write '{select * from chunk})
        nil))

  (d/q db-write '{select * from chunk where [= id 0]})

  ;; Vector of longs encoded as edn
  ;; 24.146393 ms
  ;; 3.9M

  ;; Byte array
  ;; 1.9M 51% smaller
  ;; 1.369069 ms 17.6x faster  
  )

(comment ;; Example migration of for changing column type
  
  (def db-write (-> @app_ :ctx :db-write))
  (d/q db-write
    ["CREATE TABLE IF NOT EXISTS newchunk(id INTEGER PRIMARY KEY, data BLOB, html BLOB)"])
  (d/q db-write ["INSERT INTO newchunk SELECT * FROM chunk"])
  (d/q db-write ["DROP TABLE chunk"])
  (d/q db-write ["ALTER TABLE newchunk RENAME TO chunk"])

  (d/q db-write '{select * from chunk where [= id 0]})

  (run!
    (fn [[id chunk]]
      (d/q db-write
        '{update chunk
          set    {html ?html}
          where  [= id ?id]}
        {:id   id
         :html  (String/.getBytes (h/html->str (Chunk id chunk)))}))
    (d/q db-write '{select * from chunk}))

  (d/q db-write
    ["SELECT DISTINCT path FROM dbstat WHERE pagetype = 'overflow';"])

  (d/q db-write
    ["SELECT * FROM pragma_page_size;"])

  (d/q db-write ["SELECT * FROM dbstat;"])

  (d/q db-write
    ["SELECT rowid, (
  length(id) + length(data) + length(html) + 8
) AS approx_payload
FROM chunk
WHERE approx_payload > 4069;"])

  (d/q db-write
    ["SELECT rowid, (
  length(id) + length(data) + length(html) + 8
) AS approx_payload
FROM chunk;"])


  )
