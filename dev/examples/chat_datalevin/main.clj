(ns examples.chat-datalevin.main
  (:gen-class)
  (:require [hyperlith.core :as h]
            [clojure.string :as str]
            [datalevin.core :as d]
            [examples.chat-datalevin.schema :refer [schema]]))

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
       :flex-direction  :column-reverse}]]))

(defn get-messages [db]
  (d/q '[:find ?id ?content ?created-at
         :where
         [?m :message/id ?id]
         [?m :message/content ?content]
         [?m :db/created-at ?created-at]
         :order-by [?created-at :desc]
         :limit 100]
    @db))

(defn render-home [{:keys [db] :as _req}]
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}]
    [:main#morph.main
     [:button
      {:data-on-click "@post('/send'); $message = ''"} "send"]
     [:input {:type "text" :data-bind "message"}]
     [:div
      (for [[id content] (get-messages db)]
        [:p {:id id} content])]]))

(defn action-send-message [{:keys [sid db] {:keys [message]} :body}]
  (when-not (str/blank? message)
    (d/transact! db
      [{:user/sid sid}
       {:message/id      (h/new-uid)
        :message/user    [:user/sid sid]
        :message/content message}])))

(def router
  (h/router
    {[:get (css :path)] (css :handler)
     [:get "/"]         (h/shim-handler   {:path "/"})
     [:post "/updates"] (h/render-handler #'render-home)
     [:post "/send"]    (h/action-handler #'action-send-message)}))

(defn db-start []
  (let [db (d/get-conn "db" schema
             {:validate-data?    true
              :closed-schema?    true
              :auto-entity-time? true})]
    (d/listen! db :refresh-on-change
      (fn [_] (h/refresh-all!)))
    db))

(defn -main [& _]
  (h/start-app
    {:router      #'router
     :db-start    db-start
     :db-stop     d/close
     :csrf-secret "fb1704df2b3484223cb5d2a79bf06a508311d8d0f03c68e724d555b6b605966d0ebb8dc54615f8d080e5fa062bd3b5bce5b6ba7ded23333bbd55deea3149b9d5"}))

(h/refresh-all!)

(comment
  (def server (-main))
  ;; (clojure.java.browse/browse-url "http://localhost:8080/")

  ;; stop server
  (let [stop (server :stop)] (stop))

  ;; query outside of handler
  (get-messages (:db server))
  ,)
