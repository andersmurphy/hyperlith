(ns app.main
  (:gen-class)
  (:require [clojure.pprint :as pprint]
            [hyperlith.core :as h]
            [clojure.string :as str]))

(def board-size 100)
(def chunk-size 10)
(def board-size-px 40000)
(def view-size 4)

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

       [:.chunk
        {:background            :white
         :display               :grid
         :gap                   :10px
         :grid-template-rows    (str "repeat(" chunk-size ", 1fr)")
         :grid-template-columns (str "repeat(" chunk-size ", 1fr)")}]

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

(defn Checkbox [[id color-class]]
  (let [checked (boolean color-class)]
    (h/html
      [:input
       {:class   color-class
        :type    "checkbox"
        :checked checked
        :data-id id}])))

(defn Chunk [x y chunk]
  (h/html
    [:div.chunk
     {:style {:grid-row y :grid-column x}}
     (into []
       (map (fn [box] (Checkbox box)))
       chunk)]))

(defn UserView [{:keys [x y] :or {x 0 y 0}} board-state]
  (second
    (reduce
      (fn [[dy view] board-row]
        [(inc dy)
         (into view
           (map-indexed (fn [dx chunk]
                          (Chunk (inc (+ x dx)) (inc (+ y dy)) chunk)))
           (subvec board-row x (min (+ x view-size) board-size)))])
      [0 []]
      (subvec board-state y (min (+ y view-size) board-size)))))

(defn Board [sid content]
  (h/html
    [:div#board.board
     {:style
      {:accent-color (class->color (h/modulo-pick colors sid))}
      :data-on-mousedown "evt.target.dataset.id &&
@post(`/tap?id=${evt.target.dataset.id}`)"}
     content]))

(defn render-home [{:keys [db sid first-render] :as _req}]
  (let [snapshot @db
        user     (get-in snapshot [:users sid])

        board (Board sid (UserView user (:board snapshot)))]
    (if first-render
      (h/html
        [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}]
        [:main#morph.main
         [:div#view.view
          {:data-on-scroll__throttle.200ms.trail.noleading
           "@post(`/scroll?x=${el.scrollLeft}&y=${el.scrollTop}`)"}
          board]
         [:h1 "One Million Checkboxes"]
         [:p "Built with ❤️ using "
          [:a {:href "https://clojure.org/"} "Clojure"]
          " and "
          [:a {:href "https://data-star.dev"} "Datastar"]
          "🚀"]
         [:p "Source code can be found "
          [:a {:href "https://github.com/andersmurphy/hyperlith/blob/master/examples/one_million_checkboxes/src/app/main.clj" } "here"]]])
      board)))

(defn action-tap-cell [{:keys [sid db] {:strs [id]} :query-params}]
  (when id
    (let [color-class (h/modulo-pick colors sid)
          [x y c]     (mapv parse-long (str/split id #"-"))]
      (swap! db update-in [:board y x c 1]
        (fn [color] (if (nil? color) color-class nil))))))

(defn action-scroll [{:keys [sid db] {:strs [x y]} :query-params}]
  (swap! db
    (fn [snapshot]
      (-> snapshot
        (assoc-in [:users sid :x]
          (max (- (int (* (/ (parse-double x) board-size-px) board-size)) 1)
            0))
        (assoc-in [:users sid :y]
          (max (- (int (* (/ (parse-double y) board-size-px) board-size)) 1)
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
      (mapv
        (fn [x]
          (mapv (fn [c]
                  ;; building the id once here leads to a 7x speed up
                  ;; generating hiccup (building strings is expensive)
                  [(str x "-" y "-" c) nil])
            (range (* chunk-size chunk-size))))
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
     :max-refresh-ms 100
     :ctx-start      ctx-start
     :ctx-stop       (fn [{:keys [game-stop]}] (game-stop))
     :csrf-secret    (h/env :csrf-secret)
     :on-error       (fn [_ctx {:keys [_req error]}]
                       (pprint/pprint error)
                       (flush))}))

;; Refresh app when you re-eval file
(h/refresh-all!)

(comment
  (do (-main) nil)
  ;; (clojure.java.browse/browse-url "http://localhost:8080/")

  ;; stop server
  (((h/get-app) :stop))

  (def db (-> (h/get-app) :ctx :db))

  (@db :users)

  ,)

;; 1. Database backed checkboxes
;; retrieving blocks might be faster

(comment
  (def db (-> (h/get-app) :ctx :db))

  (user/bench (do (UserView {:x 10 :y 10} (@db :board)) nil))
  )
