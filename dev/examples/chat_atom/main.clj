(ns examples.chat-atom.main
  (:gen-class)
  (:require [hyperlith.core :as h]
            [clojure.string :as str]))

(defn get-messages [db]
  (@db :messages))

(defn render-home [{:keys [db] :as _req}]
  (h/html-str
    [:main#morph
     [:div
      (for [[id content] (get-messages db)]
        [:p {:id id} content])]
     [:input {:type "text" :data-bind "message"}]
     [:button
      {:data-on-click "@post('/send'); $message = ''"} "send"]]))

(defn action-send-message [{:keys [_sid db] {:keys [message]} :body}]
  (when-not (str/blank? message)
    (swap! db update :messages conj [(h/new-uid) message])))

(def router
  (h/router
    {[:get "/"]         (h/shim-handler   {:path "/"})
     [:post "/updates"] (h/render-handler #'render-home)
     [:post "/send"]    (h/action-handler #'action-send-message)}))

(defn db-start []
  (let [db_ (atom {:messages []})]
    (add-watch db_ :refresh-on-change (fn [& _] (h/refresh-all!)))
    db_))

(defn -main [& _]
  (h/start-app
    {:router         #'router
     :max-refresh-ms 100
     :db-start       db-start
     :db-stop        (fn [_db] nil)
     :csrf-secret    "fb1704df2b3484223cb5d2a79bf06a508311d8d0f03c68e724d555b6b605966d0ebb8dc54615f8d080e5fa062bd3b5bce5b6ba7ded23333bbd55deea3149b9d5"}))

(comment
  (def server (-main))
  ;; (clojure.java.browse/browse-url "http://localhost:8080/")

  ;; stop server
  (let [stop (server :stop)] (stop))

  ;; query outside of handler
  (get-messages (:db server))
  ,)

;; CAS
