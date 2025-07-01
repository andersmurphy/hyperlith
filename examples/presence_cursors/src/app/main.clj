(ns app.main
  (:gen-class)
  (:require [hyperlith.core :as h :refer [defaction defview]]))

(def css
  (h/static-css
    [["*, *::before, *::after"
      {:box-sizing :border-box
       :margin     0
       :padding    0}]

     [:.cursor-area
      {:user-select :none
       :height      :100dvh
       :width       "100%"}]

     [:.cursor
      {:position   :absolute
       :transition "all 0.2s ease-in-out"}]]))

(def cursors
  (h/cache
    (fn [db]
      (for [[sid [x y]] @db]
        [:div.cursor
         {:id    (h/digest sid)
          :style {:left (str x "px") :top (str y "px")}}
         "🚀"]))))

(defaction handle-user-cursor-position
  [{:keys [sid db] {:keys [x y]} :body}]
  (when (and x y)
    (swap! db assoc sid [x y])))

(def shim-headers
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href css}]))

(defview handler-home {:path         "/"
                       :shim-headers shim-headers
                       :on-close
                       (fn [{:keys [sid db]}] (swap! db dissoc sid))}
  [{:keys [db] :as _req}]
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href css}]
    [:main#morph.main {:data-signals-x__ifmissing 0
                       :data-signals-y__ifmissing 0}
     [:div.cursor-area
      {:data-on-mousemove__debounce.100ms
       (str "$x = evt.clientX; $y = evt.clientY; @post('"
         handle-user-cursor-position
         "')")}
      (cursors db)]]))

(defn ctx-start []
  (let [db_ (atom {})]
    (add-watch db_ :refresh-on-change (fn [& _] (h/refresh-all!)))
    {:db db_}))

(defn -main [& _]
  (h/start-app
    {:max-refresh-ms 100
     :ctx-start       ctx-start
     :ctx-stop        (fn [_db] nil)
     :csrf-secret    (h/env :csrf-secret)}))

;; Refresh app when you re-eval file
(h/refresh-all!)

(comment
  (-main)
  ;; (clojure.java.browse/browse-url "http://localhost:8080/")

  ;; stop server
  (((h/get-app) :stop))

  (-> (h/get-app) :ctx :db)

  (reset! (-> (h/get-app) :ctx :db) {})

  ;; Example backend driven cursor test
  (def wraped-router (-> (h/get-app) :wraped-router))
  
  (doseq [_x (range 10000)]
    (Thread/sleep 1)
    (wraped-router
      {:headers
       {"accept-encoding" "br"
        "cookie"          "__Host-sid=5SNfeDa90PhXl0expOLFGdjtrpY; __Host-csrf=3UsG62ic9wLsg9EVQhGupw"
        "content-type"    "application/json"}
       :request-method :post
       :uri            handle-user-cursor-position
       :body           {:csrf "3UsG62ic9wLsg9EVQhGupw"
                        :x    (rand-nth (range 1 400 20))
                        :y    (rand-nth (range 1 400 20))}}))

  ,)
