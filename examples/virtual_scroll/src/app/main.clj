(ns app.main
  (:gen-class)
  (:require [hyperlith.core :as h :refer [defaction defview]]
            [hyperlith.extras.sqlite :as d]
            [hyperlith.extras.ui.virtual-scroll :as vs]
            [clj-async-profiler.core :as prof]))

(def row-height 20)

(def css
  (h/static-css
    [["*, *::before, *::after"
      {:box-sizing :border-box
       :margin     0
       :padding    0}]

     [:html
      {:font-family "Arial, Helvetica, sans-serif"
       :font-size   :1.0rem
       :color       :black
       :background  :white}]

     [:.main
      {:height         :100dvh
       :margin-inline  :auto
       :padding-block  :2dvh
       :display        :flex
       :width          "min(100% - 2rem , 60rem)"
       :gap            :5px
       :flex-direction :column}]

     [:.row
      {:display               :grid
       :grid-template-columns (str "repeat(" 4 ", auto)")}]]))

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

(defaction handler-scroll-component-1
  [{:keys [sid tabid tx-batch!] {:strs [x]} :query-params}]
  (when-let [x (int (parse-long x))]
    (tx-batch!
      (fn [db]
        (update-tab-data! db sid tabid
          #(assoc % :x (max x 0)))))))

(defaction handler-scroll-component-2
  [{:keys [sid tabid tx-batch!] {:strs [y]} :query-params}]
  (when-let [y (int (parse-long y))]
    (tx-batch!
      (fn [db]
        (update-tab-data! db sid tabid
          #(assoc % :y (max y 0)))))))

(defaction handler-resize-component-1
  [{:keys [sid tabid tx-batch!] {:strs [h w]} :query-params}]
  (let [height (int (parse-long h))
        width  (int (parse-long w))]
    (when (and height width)
      (tx-batch!
        (fn [db]
          (update-tab-data! db sid tabid
            #(assoc %
               :height-1 (max height 0)
               :width-1 (max width 0))))))))

(defaction handler-resize-component-2
  [{:keys [sid tabid tx-batch!] {:strs [h w]} :query-params}]
  (let [height (int (parse-long h))
        width  (int (parse-long w))]
    (when (and height width)
      (tx-batch!
        (fn [db]
          (update-tab-data! db sid tabid
            #(assoc %
               :height-2 (max height 0)
               :width-2 (max width 0))))))))

(defn Row [id [a b c :as _data]]
  (h/html [:div.row {:id (str "y" id)}
           [:div nil id] [:div nil a] [:div nil b] [:div nil c]]))

(defn row-builder [db {:keys [y-offset-items y-rendered-items]}]
  (->> (d/q db
         '{select [id data]
           from   row
           offset ?offset
           limit  ?limit}
         {:offset y-offset-items
          :limit  y-rendered-items})
    (mapv (fn [[id row]] (Row id row)))))

(defn Col [id [_a _b _c :as _data]]
  (h/html [:div {:id (str "x" id)}
           [:div nil id]]))

(defn col-builder [db {:keys [x-offset-items x-rendered-items]}]
  (->> (d/q db
         '{select [id data]
           from   row
           offset ?offset
           limit  ?limit}
         {:offset x-offset-items
          :limit  x-rendered-items})
    (mapv (fn [[id row]] (Col id row)))))

(defn row-count [db]
  (-> (d/q db '{select [[[count *]]] from row})
    first))

(def shim-headers
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href css}]
    [:title nil "One billion checkboxes"]
    [:meta {:content "So many checkboxes" :name "description"}]))

(defview handler-root
  {:path "/" :shim-headers shim-headers :br-window-size 19}
  [{:keys [db sid tabid] :as _req}]
  (let [tab-data (get-tab-data db sid tabid)]
    (h/html
      [:link#css {:rel "stylesheet" :type "text/css" :href css}]
      [:main#morph.main
       [:div#foo1 {:style {:height :10vh}}
        [::vs/virtual#view-x
         {:v/x
          {:item-size          100
           :max-rendered-items 1000
           :item-count-fn      (partial row-count db)
           :view-size          (:width-1 tab-data)
           :scroll-pos         (:x tab-data)}
          :v/item-fn             (partial col-builder db)
          :v/scroll-handler-path handler-scroll-component-1
          :v/resize-handler-path handler-resize-component-1}]]
       [:div#foo2 {:style {:height :90vh}}
        [::vs/virtual#view-y
         {:v/y
          {:item-size          20
           :max-rendered-items 1000
           :item-count-fn      (partial row-count db)
           :view-size          (:height-2 tab-data)
           :scroll-pos         (:y tab-data)}
          :v/item-fn             (partial row-builder db)
          :v/scroll-handler-path handler-scroll-component-2
          :v/resize-handler-path handler-resize-component-2}]]])))

(defn initial-table-db-state! [db]
  (let [;; chrome browsers seems to cut this off at 167771 items?
        number-of-rows 200000
        table-range    (range number-of-rows)]
    (d/with-write-tx [db db]
      (run! (fn [x]
              (d/q db
                '{insert-into row
                  values      [{id   ?row-id
                                data ?random-row}]}
                {:row-id     x
                 :random-row [(random-uuid)
                              (rand-nth ["in" "out"])
                              (rand-int 100000)]}))
        table-range)))
  nil)

(defn migrations [db]
  ;; Note: all this code must be idempotent
  ;; Create tables
  (println "Running migrations...")
  (d/q db
    ["CREATE TABLE IF NOT EXISTS row(id INT PRIMARY KEY, data BLOB)"])
  (d/q db
    ["CREATE TABLE IF NOT EXISTS session(id TEXT PRIMARY KEY, data BLOB) WITHOUT ROWID"])
  (when-not (d/q db '{select [id] from row limit 1})
    (initial-table-db-state! db)))

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
                    (d/with-write-tx [db writer]
                      (run! (fn [thunk] (thunk db)) thunks))
                    (h/refresh-all!))
                  {:run-every-ms 100})}))

(defn ctx-stop [ctx]
  (.close (:db-write ctx))
  (.close (:db-read ctx)))

(defonce app_ (atom nil))

(defn -main [& _]
  (reset! app_
    (h/start-app
      {:ctx-start   ctx-start
       :ctx-stop    ctx-stop
       :csrf-secret (h/env :csrf-secret)})))

;; Refresh app when you re-eval file
(h/refresh-all!)

(comment
  (do (-main) nil)
  ;; (clojure.java.browse/browse-url "https://localhost:3030/")


  ;; stop server
  ((@app_ :stop))

  (def db (-> @app_ :ctx :db))

  ,)

(comment ;; Profiling
  (prof/start)
  (prof/stop)
  (prof/serve-ui 7777)
  ;; (clojure.java.browse/browse-url "http://localhost:7777/")
  )
