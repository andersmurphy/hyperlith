(ns app.main
  (:gen-class)
  (:require [clojure.pprint :as pprint]
            [hyperlith.core :as h]))

(def board-size 1000)
(def board-size-px 40000)
(def view-size 50)

(def colors
  [:r :b :g :o :f :p])

(def class->color
  {:r :red :b :blue :g :green :o :orange :f :fuchsia :p :purple})

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
         :margin-inline  :auto
         :padding-block  :2dvh
         :display        :flex
         :width          "min(100% - 2rem , 40rem)"
         :gap            :5px
         :flex-direction :column}]

       [:.view
        {:overflow        :scroll
         :overflow-anchor :none
         :width           "min(100% - 2rem , 40rem)"
         :aspect-ratio    "1/1"}]

       [:.board
        {:background            :white
         :width                 board-size-px
         :display               :grid
         :aspect-ratio          "1/1"
         :gap                   :10px
         :grid-template-rows    (str "repeat(" board-size ", 1fr)")
         :grid-template-columns (str "repeat(" board-size ", 1fr)")}]

       [:.r
        {:accent-color :red}]

       [:.o
        {:accent-color :orange}]

       [:.g
        {:accent-color :green}]

       [:.b
        {:accent-color :blue}]

       [:.p
        {:accent-color :purple}]

       [:.f
        {:accent-color :fuchsia}]])))

(defn checkbox [idx checked color-class]
  (h/html
    [:input
     {:class   color-class
      :type    "checkbox"
      :style   {:grid-row    (inc (quot idx board-size))
                :grid-column (inc (rem idx board-size))}
      :checked checked
      :data-id (str "c" idx)}]))

(defn user-view [{:keys [x y] :or {x 0 y 0}} board-state]  
  (reduce
    (fn [view board-row]
      (into view (subvec board-row x (min (+ x view-size) board-size))))
    []
    (subvec board-state y (min (+ y view-size) board-size))))

(defn render-home [{:keys [db sid] :as _req}]
  (let [snapshot @db
        user     (get-in snapshot [:users sid])
        view     (user-view user (:board snapshot))]
    (h/html
      [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}]
      [:main#morph.main
       {:style {:accent-color (class->color (h/modulo-pick colors sid))}}
       [:div#view.view
        {:data-on-scroll__throttle.400ms.trail.noleading
         "@post(`/scroll?x=${el.scrollLeft}&y=${el.scrollTop}`)"}
        [:div.board
         {:data-on-mousedown "evt.target.dataset.id &&
@post(`/tap?id=${evt.target.dataset.id}`)"}
         view]]
       [:h1 "One Million Checkboxes"]
       [:p "Built with ❤️ using "
        [:a {:href "https://clojure.org/"} "Clojure"]
        " and "
        [:a {:href "https://data-star.dev"} "Datastar"]
        "🚀"]
       [:p "Source code can be found "
        [:a {:href "https://github.com/andersmurphy/hyperlith/blob/master/examples/one_million_checkboxes/src/app/main.clj" } "here"]]])))

(defn action-tap-cell [{:keys [sid db] {:strs [id]} :query-params}]
  (when id
    (let [color-class (h/modulo-pick colors sid)
          idx         (parse-long (subs id 1))
          y           (int (/ idx board-size))
          x           (int (mod idx board-size))]
      (swap! db update-in [:board y x]
        (fn [box]
          (checkbox idx (not (re-find #"checked" (str box)))
            color-class))))))

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
                                {:br-window-size 19})
     [:post "/scroll"]        (h/action-handler #'action-scroll)
     [:post "/tap"]           (h/action-handler #'action-tap-cell)}))

(defn initial-board-state []
  (mapv
    (fn [y]
      (mapv (fn [x] (checkbox (+ (* y board-size) x) false nil))
        (range board-size)))
    (range board-size)))

(defn ctx-start []
  (let [db_ (atom {:board (initial-board-state)
                   :users {}})]
    (add-watch db_ :refresh-on-change
      (fn [_ _ old-state new-state]
        ;; Only refresh if state has changed
        (when-not (= old-state new-state)
          (h/refresh-all!))))
    {:db db_}))

(defn -main [& _]
  (h/start-app
    {:router         #'router
     :max-refresh-ms 200
     :ctx-start      ctx-start
     :ctx-stop       (fn [{:keys [game-stop]}] (game-stop))
     :csrf-secret    (h/env :csrf-secret)
     :on-error       (fn [_ctx {:keys [_req error]}]
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
