(ns app.main
  (:gen-class)
  (:require [clojure.string :as str]
            [hyperlith.core :as h :refer [defaction defview]]
            [hyperlith.extras.ui.virtual-scroll-v2 :as vs]))

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
       :flex-direction  :column}]

     [:.chat
      {:display        :flex
       :flex-direction :column}]]))

(defn get-messages [db]
  (reverse (@db :messages)))

(defn get-message-count [db]
  (count (@db :messages)))

(defn messages [db {:keys [offset limit]}]
  (->> (get-messages db)
    (drop offset)
    (take limit)
    (map-indexed
      (fn [i [id content]]
        [:div {:id id}
         [:p {} "=============="]
         [:h1 {} (+ offset i)]
         [:p {} content]]))))

(defaction handler-send-message [{:keys [_sid db] {:keys [message]} :body}]
  (when-not (str/blank? message)
    (swap! db update :messages conj [(h/new-uid) message])
    (h/patch-signals {:message ""})))

(defaction handler-virtual
  [{:keys                               [_sid db tabid]
    {:strs [idx translate intersect y]} :query-params}]
  (swap! db
    #(assoc-in % [tabid :virtual]
       {:idx       (h/try-parse-long idx)
        :translate (h/try-parse-long translate)
        :intersect intersect
        :y         (h/try-parse-long y)})))

(def shim-headers
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href css}]
    [:title nil "Chat"]
    [:meta {:content "Chat app" :name "description"}]))

(defview handler-home {:path "/" :shim-headers shim-headers}
  [{:keys [db tabid] :as _req}]
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href css}]
    [:main#morph.main
     [:div.chat
      [:input {:type "text" :data-bind "message"}]
      [:button
       {:data-on:click (str "@post('" handler-send-message "')")} "send"]]
     [::vs/virtual#foo
      {:v/handler-path       handler-virtual
       :v/handler-data       (get-in @db [tabid :virtual])
       :v/item-fn            (partial messages db)
       :v/item-count-fn      (partial get-message-count db)
       :v/min-item-size      100
       :v/max-rendered-items 100}]]))

(defn ctx-start []
  (let [db_ (atom {:messages []})]
    (add-watch db_ :refresh-on-change (fn [& _] (h/refresh-all!)))
    {:db db_}))

(defn -main [& _]
  (h/start-app
    {:ctx-start      ctx-start
     :ctx-stop       (fn [_state] nil)}))

;; Refresh app when you re-eval file
(h/refresh-all!)

(comment
  (def app (-main))
  ;; (clojure.java.browse/browse-url "https://localhost:3030/")

  (def db (-> app :ctx :db))

  (run!
    (fn [message] (swap! db update :messages conj [(h/new-uid) message]))
    (repeatedly
      99
      (fn []
        (rand-nth
          ["foo" "Sed ut perspiciatis, unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam eaque ipsa, quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt, explicabo. Nemo enim ipsam voluptatem, quia voluptas sit, aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos, qui ratione voluptatem sequi nesciunt, neque porro quisquam est, qui dolorem ipsum, quia dolor sit amet consectetur adipisci[ng] velit, sed quia non numquam [do] eius modi tempora inci[di]dunt, ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum[d] exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? [D]Quis autem vel eum i[r]ure reprehenderit, qui in ea voluptate velit esse, quam nihil molestiae consequatur, vel illum, qui dolorem eum fugiat, quo voluptas nulla pariatur?"
           "bam" "baz"
           "ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum[d] exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? [D]Quis autem vel eum i[r]ure reprehenderit, qui in ea voluptate velit esse, quam nihil molestiae consequatur, vel illum, qui dolorem eum fugiat, quo voluptas nulla pariatur?"
           "ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad  consequatur, vel illum, qui dolorem eum fugiat, quo voluptas nulla pariatur?"]))))

  ;; stop server
  ((app :stop))

  ;; query outside of handler
  (get-messages (-> app :ctx :db))

  ,)
