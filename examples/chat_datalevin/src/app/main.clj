(ns app.main
  (:gen-class)
  (:require [hyperlith.core :as h]
            [clojure.string :as str]
            [datalevin.core :as d]
            [app.schema :refer [schema]]))

(def css
  (h/static-css
    [["*, *::before, *::after"
      {:box-sizing :border-box
       :margin     0
       :padding    0}]

     [:.main
      {:height          :100dvh
       :width           "min(100% - 2rem , 40rem)"
       :margin-inline   :auto
       :padding-block   :2dvh
       :overflow-y      :scroll
       :scrollbar-width :none
       :display         :flex
       :gap             :3px
       :flex-direction  :column-reverse}]

     [:.chat
      {:display        :flex
       :flex-direction :column}]]))

(defn get-messages [q]
  (q '[:find ?id ?content ?created-at
       :where
       [?m :message/id ?id]
       [?m :message/content ?content]
       [?m :db/created-at ?created-at]
       :order-by [?created-at :desc]
       :limit 100]))

(def messages
  (h/cache
    (fn [q]
      (for [[id content] (get-messages q)]
        [:p {:id id} content]))))

(defn render-home [{:keys [q] :as _req}]
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}]
    [:main#morph.main
     [:div.chat
      [:input {:type "text" :data-bind "message"}]
      [:button
       {:data-on-click "@post('/send')"} "send"]]
     (messages q)]))

(defn action-send-message [{:keys [sid tx!] {:keys [message]} :body}]
  (when-not (str/blank? message)
    (tx! [{:user/sid sid}
          {:message/id      (h/new-uid)
           :message/user    [:user/sid sid]
           :message/content message}])
    (h/signals {:message ""})))

(def default-shim-handler
  (h/shim-handler
    (h/html
      [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}]
      [:title nil "Chat"]
      [:meta {:content "Chat app" :name "description"}])))

(def router
  (h/router
    {[:get (css :path)] (css :handler)
     [:get  "/"]        default-shim-handler
     [:post "/"]        (h/render-handler #'render-home)
     [:post "/send"]    (h/action-handler #'action-send-message)}))

(defn ctx-start []
  (let [db (d/get-conn "db" schema
             {:validate-data?    true
              :closed-schema?    true
              :auto-entity-time? true})]
    (d/listen! db :refresh-on-change (fn [& _] (h/refresh-all!)))
    {:q   (fn [query & args] (apply d/q query @db args))
     :tx! (fn [tx-data] (d/transact-async db tx-data))
     :db  db}))

(defn -main [& _]
  (h/start-app
    {:router         #'router
     :max-refresh-ms 100
     :ctx-start      ctx-start
     :ctx-stop       (fn [{:keys [db]}] (d/close db))
     :csrf-secret    (h/env :csrf-secret)}))

(h/refresh-all!)

(comment
  (-main)
  ;; (clojure.java.browse/browse-url "http://localhost:8080/")

  ;; stop server
  (((h/get-app) :stop))

  ;; query outside of handler
  (get-messages (-> (h/get-app) :ctx :q))
  ,)
