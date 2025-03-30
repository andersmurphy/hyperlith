(ns app.main
  (:gen-class)
  (:require [hyperlith.core :as h]
            [hyperlith.extras.datalog :as d]
            [clojure.string :as str]
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

(defn get-messages [db]
  (d/q '[:find ?id ?content ?created-at
         :where
         [?m :message/id ?id]
         [?m :message/content ?content]
         [?m :db/created-at ?created-at]
         :order-by [?created-at :desc]
         :limit 100]
    @db))

(def messages
  (h/cache
    (fn [db]
      (for [[id content] (get-messages db)]
        [:p {:id id} content]))))

(defn render-home [{:keys [db] :as _req}]
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}]
    [:main#morph.main
     [:div.chat
      [:input {:type "text" :data-bind "message"}]
      [:button
       {:data-on-click "@post('/send')"} "send"]]
     (messages db)]))

(defn action-send-message [{:keys [sid db] {:keys [message]} :body}]
  (when-not (str/blank? message)
    (let [user-id (or (d/q '[:find ?user-id .
                             :in $ ?sid
                             :where [?s :session/id ?sid]
                             [?s :session/user ?u]
                             [?u :user/id ?user-id]]
                        @db sid)
                    (h/new-uid))]
      (d/tx! db [{:user/id user-id}
                 {:session/id   sid
                  :session/user [:user/id user-id]}
                 {:message/id      (h/new-uid)
                  :message/user    [:user/id user-id]
                  :message/content message}]))
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

(defn -main [& _]
  (h/start-app
    {:router         #'router
     :max-refresh-ms 100
     :ctx-start      (fn [] (-> (d/ctx-start "db" schema)))
     :ctx-stop       (fn [ctx] (d/ctx-stop ctx))
     :csrf-secret    (h/env :csrf-secret)
     :on-error       #'d/log-on-error}))

(h/refresh-all!)

(comment
  (-main)
  ;; (clojure.java.browse/browse-url "http://localhost:8080/")

  ;; stop server
  (((h/get-app) :stop))
  
  ,)

(comment
  ;; query outside of handler
  (def db (-> (h/get-app) :ctx :db))
  
  (d/q '[:find (pull ?e [*])
         :where [?e :error/id _]]
    @db)

  (d/q '[:find ?e (count ?ee)
         :where
         [?e :error/id _]
         [?ee :error-event/error ?e]]
    @db)

  (d/q '[:find ?e (count ?ee)
         :where
         [?e :error/id _]
         [?ee :error-event/error ?e]]
    @db)

  (d/q '[:find (count ?e)
         :where
         [?e :user/id _]]
    @db)

  (d/q '[:find (pull ?u [*])
         :in $ ?sid
         :where [?s :session/id ?sid]
         [?s :session/user ?u]]
    @db "5hv_MCnra7PpeKICamsMxALYLG4")

  (let [bar (atom nil)]
    (h/try-log {}
      (throw (ex-info "boom" {:data bar}))))
  )
