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

     [:.pe-none
      {:pointer-events :none
       :user-select    :none}]

     [:html
      {:font-family "Arial, Helvetica, sans-serif"}]

     [:.main
      {:height        :100dvh
       :width         "min(100% - 2rem , 40rem)"
       :margin-inline :auto
       :padding-block :2dvh
       :display       :grid
       :place-items   :center}]

     [:.counter
      {:text-align :center
       :font-size  :50px}]

     ["@keyframes pop"
      "{  0% {transform: scale(1);}
           25% {transform: scale(0.8);}
          100% {transform: scale(1);}}"]

     [:.pop
      {;; Animation that depresses the element
       :animation      "pop .3s ease"
       ;; Disable element until this class is removed
       :pointer-events :none}]

     [:.button
      {:background    :white
       :text-align :center
       :font-size     :1.2rem
       :border-radius :0.15em
       :border        "0.15em solid currentColor"
       :padding       :5px}]]))

(def global-session-id "global")

(defn get-session-data [db sid]
  (-> (d/q db '{select [data]
                 from   session
                 where  [= id ?sid]}
        {:sid sid})
    first))

(defn update-session-data! [db sid update-fn]
  (let [old-data (get-session-data db sid)
        new-data (update-fn old-data)]
    (if old-data
      (d/q db '{update session
                set    {data ?new-data}
                where  [= id ?sid]}
        {:sid sid :new-data new-data})
      (d/q db '{insert-into session
                values      [{:id   ?sid
                              :data ?new-data}]}
        {:sid sid :new-data new-data}))))

(defn safe-inc [x]
  (if x (inc x) 0))

(def shim-headers
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href css}]
    [:title nil "One billion checkboxes"]
    [:meta {:content "So many checkboxes" :name "description"}]))

(defaction handler-increment [{:keys [sid tabid tx-batch!]}]
  (tx-batch!
    (fn [db _]
      (update-session-data! db global-session-id
        (fn [data] (update data :counter safe-inc)))
      (update-session-data! db sid
        (fn [data] (update data :counter safe-inc)))
      (update-session-data! db sid
        (fn [data] (update-in data [:tabs tabid :counter] safe-inc))))))

(defview handler-root
  {:path "/" :shim-headers shim-headers :br-window-size 19}
  [{:keys [db sid tabid] :as _req}]
  (let [global-data  (get-session-data db global-session-id)
        session-data (get-session-data db sid)
        tab-data     (get-in session-data [:tabs tabid])]
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
          "setTimeout(() => evt.target.classList.remove('pop'), 300)"
          "}")}
       [:div
        [:div.counter
         [:p nil (str "global counter: "  (or (:counter global-data) 0))]]
        [:div.counter
         [:p nil (str "session counter: " (or (:counter session-data) 0))]]
        [:div.counter
         [:p nil (str "tab counter: "     (or (:counter tab-data) 0))]]
        [:div.button {:data-action handler-increment}
        [:strong.pe-none "click me"]]]])))

(defn migrations [db]
  ;; Note: all this code must be idempotent
  ;; Create tables
  (println "Running migrations...")
  (d/q db
    ["CREATE TABLE IF NOT EXISTS session(id TEXT PRIMARY KEY, data BLOB) WITHOUT ROWID"])
  (when-not (d/q db '{select                    [id]  from session where
                      [= id ?global-session-id] limit 1}
              {:global-session-id global-session-id})
    (d/q db '{insert-into session
              values      [{:id   ?sid
                            :data ?new-data}]}
      {:sid global-session-id :new-data {}})))

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
                        (run! (fn [thunk] (thunk db chunk-cache)) thunks)))
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
