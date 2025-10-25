(ns app.main
  (:gen-class)
  (:require [clojure.string :as str]
            [hyperlith.core :as h :refer [defaction defview]]))

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
  (reverse (@db :messages)))

(defn messages [db]
  (for [[id content] (get-messages db)]
    [:p {:id id} content]))

(defaction handler-send-message [{:keys [_sid db] {:keys [message]} :body}]
  (when-not (str/blank? message)
    (swap! db update :messages conj [(h/new-uid) message])
    (h/patch-signals {:message ""})))

(def shim-headers
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href css}]
    [:title nil "Chat"]
    [:meta {:content "Chat app" :name "description"}]))

(defview handler-home {:path "/" :shim-headers shim-headers}
  [{:keys [db] :as _req}]
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href css}]
    [:main#morph.main
     [:div.chat
      [:input {:type "text" :data-bind "message"}]
      [:button
       {:data-on:click (str "@post('" handler-send-message "')")} "send"]]
     (messages db)]))

(defn ctx-start []
  (let [db_ (atom {:messages []})]
    (add-watch db_ :refresh-on-change (fn [& _] (h/refresh-all!)))
    {:db db_}))

(defn -main [& _]
  (h/start-app
    {:max-refresh-ms 100
     :ctx-start      ctx-start
     :ctx-stop       (fn [_state] nil)
     :csrf-secret    (h/env :csrf-secret)}))

;; Refresh app when you re-eval file
(h/refresh-all!)

(comment
  (def app (-main))
  ;; (clojure.java.browse/browse-url "http://localhost:8080/")

  ;; stop server
  ((app :stop))

  ;; query outside of handler
  (get-messages (-> app :ctx :db))
  ,)
