(ns app.main
  (:gen-class)
  (:require [clojure.pprint :as pprint]
            [hyperlith.core :as h]
            [hyperlith.extras.datalevin :as d]
            [clojure.string :as str]))

;; (* 72 72 14 14) -> 1016064
;; (* 226 226 14 14) -> 10010896 over 10 million checkboxes?
;; (* 2259 2259 14 14) -> 1 000 203 876 over 1 billion checkboxes?
;; (* 2259 2259) 5 million blocks

;; TODO: database
;; TODO: 1 billion

(def board-size 226)
(def chunk-size 14)
(def board-size-px 140000)
(def view-size 3)

(def schema
  (merge
    #:chunk
    {:id {:db/unique      :db.unique/identity
          :db/valueType   :db.type/string
          :db/cardinality :db.cardinality/one}}
    
    #:cell
    {:id    {:db/unique      :db.unique/identity
             :db/valueType   :db.type/string
             :db/cardinality :db.cardinality/one}
     :chunk {:db/valueType   :db.type/ref
             :db/cardinality :db.cardinality/one}
     :check {:db/valueType   :db.type/keyword
             :db/cardinality :db.cardinality/one}}))

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

(defn Checkbox [{:cell/keys [id check]}]
  (let [checked (not= check :unchecked)]
    (h/html
      [:input
       {:class   check
        :type    "checkbox"
        :checked checked
        :data-id id}])))

(defn Chunk [{id          :chunk/id
              chunk-cells :cell/_chunk}]
  (let [[x y] (str/split id #"-")
        x     (inc (parse-long x))
        y     (inc (parse-long y))]
    (h/html
      [:div.chunk
       {:style {:grid-row y :grid-column x}}
       (mapv (fn [box] (Checkbox box)) chunk-cells)])))

(defn xy->chunk-ids [x y]
  (-> (for [x (range x (+ x 3))
            y (range y (+ y 3))]
        (str  x "-" y))
    vec))

(defn UserView [{:keys [x y] :or {x 0 y 0}} db]
  (->> (d/q '[:find [(pull ?ch [:chunk/id
                              {:cell/_chunk [:cell/id :cell/check]}]) ...]
             :in $ [?chunk-id ...]
             :where
             [?ch :chunk/id ?chunk-id]]
         @db (xy->chunk-ids x y))
    (mapv Chunk)))

(defn Board [sid content]
  (h/html
    [:div#board.board
     {:style
      {:accent-color (class->color (h/modulo-pick colors sid))}
      :data-on-mousedown "evt.target.dataset.id &&
@post(`/tap?id=${evt.target.dataset.id}`)"}
     content]))

(defn scroll-offset-js [n]
  (str "Math.round((" n "/" board-size-px ")*" board-size "-1)"))

(def on-scroll-js
  (str
    "let x = " (scroll-offset-js "el.scrollLeft") ";"
    "let y = " (scroll-offset-js "el.scrollTop") ";"
    "let change = x !== $x || y !== $y;"
    "$x = x; $y = y;"
    "change && @post(`/scroll`)"))

(defn render-home [{:keys [db sid tab tabid first-render] :as _req}]
  (let [user  (get-in @tab [sid tabid] tab)
        board (Board sid (UserView user db))]
    (if first-render
      (h/html
        [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}]
        [:main#morph.main {:data-signals-x "0" :data-signals-y "0"}
         [:div#view.view
          {:data-on-scroll__throttle.100ms.trail.noleading on-scroll-js}
          board]
         [:h1 "Ten Million Checkboxes"]
         [:p "Built with ❤️ using "
          [:a {:href "https://clojure.org/"} "Clojure"]
          " and "
          [:a {:href "https://data-star.dev"} "Datastar"]
          "🚀"]
         [:p "Source code can be found "
          [:a {:href "https://github.com/andersmurphy/hyperlith/blob/master/examples/ten_million_checkboxes/src/app/main.clj" } "here"]]])
      board)))

(defn action-tap-cell [{:keys [sid db] {:strs [id]} :query-params}]
  (when id
    (let [color-class (h/modulo-pick colors sid)
          check       (d/q '[:find ?check .
                             :in $ ?cell-id
                             :where
                             [?c :cell/id ?cell-id]
                             [?c :cell/check ?check]]
                        @db id)]
      @(d/tx! db [{:cell/id    id
                   :cell/check (if (= :unchecked check)
                                 color-class
                                 :unchecked)}]))))

(defn action-scroll [{:keys [sid tabid tab] {:keys [x y]} :body}]
  (swap! tab
    (fn [snapshot]
      (-> snapshot
        (assoc-in [sid tabid :x] (max (int x) 0))
        (assoc-in [sid tabid :y] (max (int y) 0))))))

(def default-shim-handler
  (h/shim-handler
    (h/html
      [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}]
      [:title nil "One Million checkboxes"]
      [:meta {:content "So many checkboxes" :name "description"}])))

(def router
  (h/router
    {[:get (css :path)] (css :handler)
     [:get  "/"]        default-shim-handler
     [:post "/"]        (h/render-handler #'render-home
                          {:br-window-size 19})
     [:post "/scroll"]  (h/action-handler #'action-scroll)
     [:post "/tap"]     (h/action-handler #'action-tap-cell)}))

(defn tab-state-ctx-start []
  (let [tab-state_ (atom {:users {}})]
    (add-watch tab-state_ :refresh-on-change
      (fn [_ _ _ _] (h/refresh-all!)))
    {:tab tab-state_}))

(defn -main [& _]
  (h/start-app
    {:router         #'router
     :max-refresh-ms 100
     :ctx-start      (fn [] (->> (tab-state-ctx-start)
                              (d/ctx-start "db" schema
                                {:auto-entity-time? false})))
     :ctx-stop       (fn [ctx] (d/ctx-stop ctx))
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

(comment
  (def db (-> (h/get-app) :ctx :db))

  (defn build-chunk [x y]
    (into [{:chunk/id (str x "-" y)}]
      (mapv (fn [c]
              {:cell/id    (str x "-" y "-" (format "%03d" c))
               :cell/check :unchecked
               :cell/chunk [:chunk/id (str x "-" y)]})
        (range (* chunk-size chunk-size)))))

  (defn initial-board-db-state! [db]
    (let [board-range (range board-size)]
      (run!
        (fn [y]
          @(peek (mapv (fn [x] (d/tx! db (build-chunk x y))) board-range))
          (println "row: " y))
        board-range))
    nil)

  (initial-board-db-state! db)
  ,)

(comment
  (def db (-> (h/get-app) :ctx :db))

  (time (do (UserView {:x 10 :y 10} db) nil))

  (user/bench (do (UserView {:x 10 :y 10} db) nil))

  (time (d/q '[:find  (pull ?e [*]) :where [?e :cell/id "2-2-048"]] @db))
  ,)
