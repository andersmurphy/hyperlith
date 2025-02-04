(ns examples.chat-atom.main
  (:gen-class)
  (:require [hyperlith.core :as h]
            [clojure.string :as str]))

(defn- to-str [s]
  (cond (keyword? s) (name s)
        (vector? s)  (->> (map to-str s)
                       (interpose " ")
                       (apply str))
        :else        (str s)))

(defn- format-rule [[k v]]
  (str
    (to-str k)
    "{"
    (reduce (fn [acc [k v]]
              (str acc (to-str k) ":" (to-str v) ";"))
      ""
      (sort-by (comp to-str key) v))
    "}"))

(def css-rules
  [["*, *::before, *::after"
    {:box-sizing  :border-box
     :margin      0
     :padding     0}]
   
   [:.main
    {:height          :100dvh
     :width           "min(100% - 2rem , 40rem)"
     :margin-inline   :auto
     :padding-block    :2dvh
     :overflow-y      :scroll
     :scrollbar-width :none
     :display         :flex
     :flex-direction  :column-reverse}]])

(def css
  (h/static-asset
    {:body (->> (map format-rule css-rules) (reduce str ""))
     :content-type "text/css"
     :gzip?        true}))

(defn get-messages [db]
  (@db :messages))

(defn render-home [{:keys [db] :as _req}]
  (h/html
    [:main#morph.main
     [:button
      {:data-on-click "@post('/send'); $message = ''"} "send"]
     [:input {:type "text" :data-bind "message"}]
     [:div
      (for [[id content] (get-messages db)]
        [:p {:id id} content])]]))

(defn action-send-message [{:keys [_sid db] {:keys [message]} :body}]
  (when-not (str/blank? message)
    (swap! db update :messages conj [(h/new-uid) message])))

(def router
  (h/router
    {[:get (css :path)] (css :handler)
     [:get "/"]         (h/shim-handler   {:path "/" :css-path (css :path)})
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


