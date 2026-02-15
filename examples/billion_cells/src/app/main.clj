(ns app.main
  (:gen-class)
  (:require [hyperlith.core :as h :refer [defaction defview]]
            [hyperlith.extras.sqlite :as d]
            [clojure.math :as math]
            [hyperlith.extras.ui.virtual-scroll :as vs]
            [clojure.string :as str]))

(def cell-width-px (* 32 4))
(def cell-height-px 32)
(def chunk-size 16)
(def chunk-width-px (* cell-width-px chunk-size))
(def chunk-height-px (* cell-height-px chunk-size))
(def board-size (->> (math/pow chunk-size 2)
                  (/ 1000000000)
                  math/sqrt
                  math/ceil
                  int))
(def board-width-px (* cell-width-px chunk-size board-size))
(def board-height-px (* cell-height-px chunk-size board-size))
(def size (* board-size chunk-size))
(def black "#000000")
(def white "#FFF1E8")

(def css
  (let [accent          "#008751"
        other           "#FF004D"]
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
         :max-height     :100dvh
         :width          :100dvw
         :max-width      :100dvw
         :padding-inline :2dvw
         :padding-block  :2dvh
         :gap            :5px
         :display        :flex
         :flex-direction :column}]

       [:.view-wrapper
        {:min-width  (str cell-width-px "px")
         :min-height (str cell-height-px "px")}]

       [:.controls-wrapper
        {:gap            :5px
         :display        :flex
         :flex-direction :column}]

       [:.chunk
        {:background               white
         :display                  :grid
         :grid-template-rows
         (str "repeat("chunk-size","cell-height-px"px)")
         :grid-template-columns
         (str "repeat("chunk-size","cell-width-px"px)")
         :grid-column              (str "span " chunk-size)
         :grid-row                 (str "span " chunk-size)
         ;; For how subgrid and contain interact see:
         ;; https://github.com/w3c/csswg-drafts/issues/7091
         :content-visibility       :auto 
         :contain                  :strict
         :contain-intrinsic-height (str (* chunk-size cell-height-px)"px")
         :contain-intrinsic-width  (str (* chunk-size cell-width-px)"px")}]

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
        {:background    white
         :font-size     :1.2rem
         :border-radius :0.15em
         :border        "0.15em solid currentColor"
         :padding       :5px}]

       [:a {:color accent}]

       [:.cell
        {:border         (str "1px solid " black)
         :font-size      :1.2rem
         :display        :grid
         :overflow       :hidden
         :white-space    :nowrap
         :place-content  :center
         :pointer-events :all}]

       [:.focus-cell
        {:background  white
         :border      (str "1px solid " black)
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

(def blank-chunk
  (-> (repeat (* chunk-size chunk-size) {})
    vec))

(defn update-chunk! [db chunk-cache chunk-id update-fn]
  (let [old-chunk (or (@chunk-cache chunk-id)
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
                      first))
        new-chunk (update-fn old-chunk)]
    (swap! chunk-cache assoc chunk-id new-chunk)))

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
  (str "$_view.scroll(" (int (* (/ x size) board-width-px))
    "," (int (* (/ y size) board-height-px)) ");"))

(defaction handler-jump
  [{:keys [_sid _tabid _tx-batch!] {:keys [jumpx jumpy]} :body}]
  (h/execute-expr (scroll-to-xy-js jumpx jumpy)))

(defaction handler-share
  [{:keys [_sid _tabid _tx-batch!] {:keys [jumpx jumpy]} :body}]
  (h/html
    [:div.toast {:data-on:load__delay.3s "el.remove()"}
     [:div.button
      [:p [:strong nil (str "X: " jumpx " Y: " jumpy)]]
      [:p [:strong "SHARE URL COPIED TO CLIPBOARD"]]]]))

(defn Cell [local-id {:keys [value focus]} sid]
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
            :maxlength                     20
            :size                          10
            :type                          "text"
            :data-on:load                  on-load
            :data-preserve-attr            "data-on:load"
            :data-bind                     "cellvalue"
            :data-on:input__debounce.200ms on-input)]]))

    focus
    (h/html
      [:div.focus-cell
       [:p.focus-other
        {:data-id       local-id
         :data-action   handler-focused}
        value]])

    :else (h/html [:p.cell
                   {:data-id       local-id
                    :data-value    value
                    :data-action   handler-focused}
                   value])))

(defn xy->chunk-id [x y]
  (+ x (* y board-size)))

(defn xy->chunk-ids
  [{:keys [x-offset-items y-offset-items x-rendered-items y-rendered-items]}]
  (-> (for [y (range y-offset-items (+ y-offset-items y-rendered-items))
            x (range x-offset-items (+ x-offset-items x-rendered-items))]
        (xy->chunk-id x y))
    vec))

(defn Chunk [chunk-id chunk-cells sid]
  (h/html
    [:div.chunk {:id          (str "chunk-" chunk-id)
                 :data-id     chunk-id
                 :data-ignore true}
     (into []
       (map-indexed (fn [local-id box] (Cell local-id box sid)))
       chunk-cells)]))

(def empty-cells
  (h/html
    (into []
      (map-indexed (fn [local-id box] (Cell local-id box nil)))
      blank-chunk)))

(defn EmptyChunk [chunk-id]
  (h/html
    [:div.chunk {:id                (str "chunk-" chunk-id)
                 :data-ignore-morph true
                 :data-ignore       true
                 :data-id           chunk-id}
     empty-cells]))

(defn UserView
  [db sid {:keys [x-offset-items y-offset-items
                  x-rendered-items y-rendered-items] :as offset-data}]
  {:corner (h/html [:div {:style {:background white}}])
   :header  (mapv (fn [x] (h/html
                            [:div {:style {:background  black
                                           :color       white
                                           :height      :40px
                                           :display     :grid
                                           :border-inline
                                           (str "1px solid " white)
                                           :place-items :center}}
                             [:h2 nil x]]))
              (range (* x-offset-items chunk-size)
                (* (+ x-offset-items x-rendered-items) chunk-size)))
   :sidebar (mapv (fn [x] (h/html
                            [:div {:style {:background  black
                                           :color       white
                                           :width       :100px
                                           :display     :grid
                                           :border-block
                                           (str "1px solid " white)
                                           :place-items :center}}
                             [:h2 nil x]]))
              (range (* y-offset-items chunk-size)
                (* (+ y-offset-items y-rendered-items) chunk-size)))
   :content
   (h/html
     [:div {:style {:display       :grid
                    :grid-template "subgrid/subgrid"
                    :grid-area     "1/1/-1/-1"}}
      (->> (xy->chunk-ids offset-data)
        (mapv (fn [chunk-id]
                (let [[[id chunk]] (d/q db '{select [id data]
                                             from   chunk
                                             where  [= id ?chunk-id]}
                                     {:chunk-id chunk-id})]
                  (if id
                    (Chunk id chunk sid)
                    (EmptyChunk chunk-id))))))])})

(def copy-xy-to-clipboard-js "navigator.clipboard.writeText(`https://cells.andersmurphy.com?x=${$jumpx}&y=${$jumpy}`)")

(def shim-headers
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href css}]
    [:title nil "One billion cells"]
    [:meta {:content "So many cells" :name "description"}]))

(defview handler-root
  {:path              "/" :shim-headers shim-headers :br-window-size 24
   :on-close          (fn [{:keys [tx-batch! sid tabid]}]
               (tx-batch! (partial remove-focus! sid tabid)))
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
        {:keys [x y height width]} tab-data]
    (h/html
      [:link#css {:rel "stylesheet" :type "text/css" :href css}]
      [:main#morph.main
       {:data-on:mousedown
        (str
          "if (evt.target.dataset.action) {"
          "evt.target.classList.add('pop');"
          "$targetid = evt.target.dataset.id;"
          "$parentid = evt.target.parentElement.dataset.id;"
          "$gparentid = evt.target.parentElement.parentElement.dataset.id;"
          "@post(`${evt.target.dataset.action}`);"
          "setTimeout(() => evt.target.classList.remove('pop'), 300)"
          "}")}
       [:div.view-wrapper
        {;; firefox sometimes preserves scroll on refresh and we don't want that
         :data-on:load (scroll-to-xy-js jump-x jump-y)}
        [::vs/virtual-table#view
         {:data-ref              "_view"
          :v/x                   {:item-size          chunk-width-px
                                  :buffer-items       1
                                  :max-rendered-items 5
                                  :scroll-pos         x
                                  :view-size          width
                                  :item-count-fn      (fn [] board-size)
                                  :chunk-size         chunk-size}
          :v/y                   {:item-size          chunk-height-px
                                  :buffer-items       2
                                  :max-rendered-items 7
                                  :scroll-pos         y
                                  :view-size          height
                                  :item-count-fn      (fn [] board-size)
                                  :chunk-size         chunk-size}
          :v/item-fn             (partial UserView db sid)
          :v/scroll-handler-path handler-scroll
          :v/resize-handler-path handler-resize}]]
       [:div.controls-wrapper
        [:div.jump
         [:h2 "X:"]
         [:input.jump-input
          {:type        "number" :data-bind "jumpx"
           :data-effect (str "$jumpx = Math.round(($view-x/" board-width-px
                          ")*" size ")")}]
         [:h2 "Y:"]
         [:input.jump-input
          {:type        "number" :data-bind "jumpy"
           :data-effect (str "$jumpy = Math.round(($view-y/" board-height-px
                          ")*" size ")")}]
         [:div.button {:data-action handler-jump}
          [:strong.pe-none "GO"]]
         [:div.button
          {:data-action       handler-share
           :data-on:mousedown copy-xy-to-clipboard-js}
          [:strong.pe-none "SHARE"]]]
        [:h1 "One Billion Cells"]
        [:p "Built using "
         [:a {:href "https://clojure.org/"} "Clojure"]
         " and "
         [:a {:href "https://data-star.dev"} "Datastar"]
         " - "
         [:a {:href "https://github.com/andersmurphy/hyperlith/blob/master/examples/billion_cells/src/app/main.clj" } "source"]
         " - "
         [:p [:a {:href "https://andersmurphy.com/about"} "blog"]]]]])))

(defn prep-chunk-fts [chunk]
  (->> (flatten chunk)
    (mapv :value)
    (str/join " ")))

(defn migrations [db]
  ;; Note: all this code must be idempotent
  ;; Create tables
  (println "Running migrations...")
  (d/q db
    ["CREATE TABLE IF NOT EXISTS chunk(id INT PRIMARY KEY, data BLOB)"])
  (d/q db
    ["CREATE TABLE IF NOT EXISTS session(id TEXT PRIMARY KEY, data BLOB) WITHOUT ROWID"])
  (d/q db
    ["CREATE VIRTUAL TABLE IF NOT EXISTS chunk_fts USING fts5(data, content='chunk', content_rowid='id');"]))

(defn ctx-start []
  (let [db-name "cells.db"
        _       (d/restore-then-replicate! db-name
                  {:s3-access-key-id     (h/env :s3-access-key-id)
                   :s3-access-secret-key (h/env :s3-access-secret-key)
                   :bucket               "hyperlith"
                   :endpoint             "https://nbg1.your-objectstorage.com"
                   :region               "nbg1"})
        {:keys [writer reader] :as db-obj}
        (d/init-db! db-name
          {:pool-size 4
           :pragma    {:foreign_keys false}})]
    (d/create-function db-obj "prep_chunk_fts" #'prep-chunk-fts
      {:deterministic? true})
    ;; Run migrations
    (migrations writer)
    {:db-obj    db-obj
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
       :ctx-stop       ctx-stop})))

;; Refresh app when you re-eval file
(h/refresh-all!)

(comment
  (do (-main) nil)
  ;; (clojure.java.browse/browse-url "https://localhost:3030/")

  ;; stop server
  ((@app_ :stop))

  (def db (-> @app_ :ctx :db))
  (d/q db '{select [[[count *]]] from session})
  (+ 7698)
  

  ,)

(comment
  ;; clear out empty chunks
  (def db-write (-> @app_ :ctx :db-write))
  (d/q db-write '{select [[[count *]]] from chunk})
  
  (run! (fn [chunk-id]
          (when (= blank-chunk (-> (d/q db-write
                                     '{select [data]
                                       from   chunk
                                       where  [= id ?chunk-id]}
                                     {:chunk-id chunk-id})
                                 first))
            (d/q db-write '{delete-from chunk
                            where       [= id ?chunk-id]}
              {:chunk-id chunk-id})))
    (range (* board-size board-size)))

  )

(comment
  (def db-write (-> @app_ :ctx :db-write))
  ;; Free up space (slow)
  ;; (time (d/q db-write ["VACUUM"]))
  ;; Checkpoint the WAL
  (d/q db-write ["PRAGMA wal_checkpoint(PASSIVE)"])
  (d/q db-write ["PRAGMA wal_checkpoint(TRUNCATE)"])
  )

(comment

  (def db-write (-> @app_ :ctx :db-write))

  
  (d/q db-write ["select data from chunk"])

  (count

    (d/q db-write ["select * from chunk_fts where chunk_fts match 'cool'"]))
  
  (d/q db-write ["select count(*) from chunk_fts"])

  (d/q db-write
    ["INSERT INTO chunk_fts(chunk_fts) VALUES('delete-all')"])
  (d/q db-write
    ["insert into chunk_fts(rowid, data) select id, prep_chunk_fts(data) from chunk"])

  )

