(ns app.main
  (:gen-class)
  (:require [clojure.pprint :as pprint]
            [hyperlith.core :as h]))

(def board-size 1000)
(def board-size-px 20000)
(def chunk-size 50)

(def css
  (let [black         :black
        board-size-px (str board-size-px "px")]
    (h/static-css
      [["*, *::before, *::after"
        {:box-sizing :border-box
         :margin     0
         :padding    0}]

       [:html
        {:font-family "Arial, Helvetica, sans-serif"
         :font-size   :18px
         :color       black}]

       [:.main
        {:height         :100dvh
         :width          "min(100% - 2rem , 30rem)"
         :margin-inline  :auto
         :padding-block  :2dvh
         :display        :flex
         :gap            :5px
         :flex-direction :column}]

       [:.view
        {:margin-inline   :auto
         :overflow        :scroll
         :overflow-anchor :none
         :width           "min(100% - 2rem , 30rem)"
         :aspect-ratio    "1/1"}]

       [:.board
        {:background            :white
         :width                 board-size-px
         :display               :grid
         :aspect-ratio          "1/1"
         :grid-template-rows    (str "repeat(" board-size ", 1fr)")
         :grid-template-columns (str "repeat(" board-size ", 1fr)")}]])))

(defn index->coordinates [idx max-cols]
  [(quot idx max-cols) (rem idx max-cols)])

(defn coordinates->index [row col max-cols]
  (+ col (* row max-cols)))

(def board-state
  (h/cache
    (fn [board-data]
      (into []
        (comp
          (map-indexed
            (fn [id checked]
              (let [[x y] (index->coordinates id board-size)]
                (h/html
                  [:input
                   {:type    "checkbox"
                    :style   {:grid-row (inc x) :grid-column (inc y)}
                    :checked (boolean checked)
                    :data-id (str "c" id)}]))))
          (partition-all board-size)
          (map vec))
        board-data))))

(defn user-view [{:keys [x y] :or {x 0 y 0}} board-state]  
  (reduce
    (fn [view board-row]
      (into view (subvec board-row x (min (+ x chunk-size) board-size))))
    []
    (subvec board-state y (min (+ y chunk-size) board-size))))

(defn game-view [snapshot sid]
  (let [user (get-in snapshot [:users sid])
        view (user-view user (board-state (:board snapshot)))]
    (h/html
      [:div#view.view
       {:data-on-scroll__debounce.100ms
        "@post(`/scroll?x=${el.scrollLeft}&y=${el.scrollTop}`)"}
       [:div.board
        {:data-on-mousedown "@post(`/tap?id=${evt.target.dataset.id}`)"}
        view]])))

(defn render-home [{:keys [db sid] :as _req}]
  (let [snapshot @db]
    (h/html
      [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}]
      [:main#morph.main
       (game-view snapshot sid)])))

(defn action-tap-cell [{:keys [_sid db] {:strs [id]} :query-params}]
  (when id
    (swap! db update-in [:board (parse-long (subs id 1))] not)))

(defn action-scroll [{:keys [sid db] {:strs [x y]} :query-params}]
  (swap! db
    (fn [snapshot]
      (-> snapshot
        (assoc-in [:users sid :x]
          (max (- (int (* (/ (parse-double x) board-size-px) board-size)) 18)
            0))
        (assoc-in [:users sid :y]
          (max (- (int (* (/ (parse-double y) board-size-px) board-size)) 18)
            0))))))

(def default-shim-handler
  (h/shim-handler
    (h/html
      [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}]
      [:title nil "One Million checkboxes"]
      [:meta {:content "So many checkboxes" :name "description"}])))

(def router
  (h/router
    {[:get (css :path)]       (css :handler)
     [:get  "/"]              default-shim-handler
     [:post "/"]              (h/render-handler #'render-home
                                {:br-window-size 18})
     [:post "/scroll"]        (h/action-handler #'action-scroll)
     [:post "/tap"]           (h/action-handler #'action-tap-cell)}))

(defn ctx-start []
  (let [db_ (atom {:board (vec (take (* board-size board-size) (repeat false)))
                   :users {}})]
    (add-watch db_ :refresh-on-change
      (fn [_ _ old-state new-state]
        ;; Only refresh if state has changed
        (when-not (= old-state new-state)
          (h/refresh-all!
            :keep-cache? (= (:board old-state) (:board new-state))))))
    {:db db_}))

(defn -main [& _]
  (h/start-app
    {:router         #'router
     :max-refresh-ms 200
     :ctx-start      ctx-start
     :ctx-stop       (fn [{:keys [game-stop]}] (game-stop))
     :csrf-secret    (h/env :csrf-secret)
     :on-error       (fn [_ctx {:keys [req error]}]
                       ;; (pprint/pprint req)
                       (pprint/pprint error)
                       (flush))}))

;; Refresh app when you re-eval file
(h/refresh-all!)

(comment
  (-main)
  ;; (clojure.java.browse/browse-url "http://localhost:8080/")

  ;; stop server
  (((h/get-app) :stop))

  (def db (-> (h/get-app) :ctx :db))

  ,)
