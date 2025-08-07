(ns app.main
  (:gen-class)
  (:require [hyperlith.core :as h :refer [defaction defview]]
            [hyperlith.extras.sqlite :as d]))

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

     [:.view
      {:overflow-y      :scroll
       :scroll-behavior :smooth
       :overflow-anchor :none
       :height          "min(100% - 2rem , 60rem)"}]

     [:.table
      {:background     :white
       :display        :grid
       :pointer-events :none}]

     [:.row
      {:display        :grid
       :grid-template-columns (str "repeat(" 4 ", auto)")}]]))

(def row-height 20)

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

(defaction handler-scroll
  [{:keys [sid tabid tx-batch!] {:keys [y]} :body}]
  (tx-batch!
    (fn [db]
      (update-tab-data! db sid tabid
        #(assoc % :y (max (int y) 0))))))

(defn y->offset [row-count y]
  (max (- (int (* (/ y (* row-count row-height)) row-count)) 100) 0))

(defn Row [y id [a b c :as _data]]
  (h/html [:div.row {:style {:grid-row y}}
           [:div id] [:div a] [:div b] [:div c]]))

(defn UserView [{:keys [y row-count] :or {y 0}} db]
  (let [current-offset (y->offset row-count y)]
    (->> (d/q db
           '{select [id data]
             from   row
             offset ?offset
             limit  300}
           {:offset current-offset})
         (map-indexed (fn [i [id row]] (Row (+ current-offset i) id row))))))

(def on-scroll-js
  (str "$y = el.scrollTop; @post(`" handler-scroll "`)"))

(def shim-headers
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href css}]
    [:title nil "One billion checkboxes"]
    [:meta {:content "So many checkboxes" :name "description"}]))

(defview handler-root
  {:path "/" :shim-headers shim-headers :br-window-size 19}
  [{:keys [db sid tabid] :as _req}]
  (let [;; TODO: make this dynamic
        row-count 100000
        tab-data         (get-tab-data db sid tabid)
        content          (UserView (assoc tab-data :row-count row-count) db)]
    (h/html
      [:link#css {:rel "stylesheet" :type "text/css" :href css}]
      [:main#morph.main
       [:div#view.view
        {:data-ref                                       "_view"
         :data-on-scroll__throttle.100ms.trail.noleading on-scroll-js}
        [:div#table.table
         {:style
          {:grid-template-rows
           (str "repeat(" row-count "," row-height "px)")}}
         content]]])))

(defn initial-table-db-state! [db]
  (let [number-of-rows 200000
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
