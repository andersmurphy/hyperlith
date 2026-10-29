(ns app.main
  (:gen-class)
  (:require
   [app.game :as game]
   [hyperlith.impl.sqlite :as d]
   [hyperlith.core :as h :refer [defaction defview defc]]))

(def board-size 50)

(def colors
  ["" "red" "blue" "green" "orange" "fuchsia" "purple"])

(def css
  (let [black           :black
        cell-transition "background 0.2s ease"]
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

       [:.board
        {:background            :white
         :width                 "min(100% - 2rem , 30rem)"
         :display               :grid
         :aspect-ratio          "1/1"
         :grid-template-rows    (str "repeat(" board-size ", 1fr)")
         :grid-template-columns (str "repeat(" board-size ", 1fr)")}]

       [:.tile
        {:border-bottom "1px solid black"
         :border-right  "1px solid black"}]

       [:.dead
        {:background :white}]

       [:.red
        {:background :red
         :transition cell-transition}]
       [:.blue
        {:background :blue
         :transition cell-transition}]
       [:.green
        {:background :green
         :transition cell-transition}]
       [:.orange
        {:background :orange
         :transition cell-transition}]
       [:.fuchsia
        {:background :fuchsia
         :transition cell-transition}]
       [:.purple
        {:background :purple
         :transition cell-transition}]])))

(defn fill-cell [board color id]
  (when ;; crude overflow check
      (<= 0 id (dec (* board-size board-size)))
    (aset-byte ^byte/1 board (int id) (int color)))
  board)

(defn fill-cross [board id sid]
  (let[user-color (-> (hash sid) (mod (dec (count colors))) inc)]
    (-> board
      (fill-cell user-color (- id board-size))
      (fill-cell user-color (- id 1))
      (fill-cell user-color id)
      (fill-cell user-color (+ id 1))
      (fill-cell user-color (+ id board-size)))))

(defaction handler-tap-cell [{::h/keys [tx! sid] {:strs [id]} :query-params}]
  (when id
    (tx! (fn [_ board]
           (fill-cross board (parse-long id) sid)))))

(def shim-headers
  [[:link {:id "css" :rel "stylesheet" :type "text/css" :href css}]
   [:title "Game of Life"]
    [:meta {:content "Conway's Game of Life" :name "description"}]])

(defc Cell [id color-class]
  {:key [Cell id color-class]}
  [:div {:class   (str (colors (int color-class)) " tile")
         :data-id id}])

(defc Board [board version]
  {:key [Board version]}
  [:div {:data-on:pointerdown
         (str "@post(`" handler-tap-cell "?id=${evt.target.dataset.id}`)")}
   [:div {:class "board"}
    (into []
      (comp
        (map-indexed
          (fn [id color-class]
            (Cell id color-class))))
      board)]])

(defview render-home {:path        "/" :shim-headers shim-headers
                      :zstd-window 20}
  [{:keys [db]} _req]
  [[:link {:id "css" :rel "stylesheet" :type "text/css" :href css}]
   [:main {:class "main" :id "morph"}
    [:h1 "Game of Life (multiplayer)"]
    [:p "Built with ❤️ using "
     [:a {:href "https://clojure.org/"} "Clojure"]
     " and "
     [:a {:href "https://data-star.dev"} "Datastar"]
     "🚀"]
    [:p "Source code can be found "
     [:a {:href "https://github.com/andersmurphy/hyperlith/blob/master/examples/game_of_life/src/app/main.clj"} "here"]
     " - " [:a {:href "https://andersmurphy.com/about"} "blog"]]
    (let [[[board version]] (d/q db '{select [data v]
                                      from   chunk
                                      where  [= id 1]
                                      limit  1}
                              (fn [stmt] [(d/blob stmt 0) (d/int stmt 1)]))]
      (Board board version))]])

(defview render-home-embed {:path "/embed" :shim-headers shim-headers}
  [{:keys [db]} _req]
  [[:link {:id "css" :rel "stylesheet" :type "text/css" :href css}]
   [:main {:class "main" :id "morph"}
    [:h1 "Game of Life (multiplayer)"]
    [:p "Built with ❤️ using Clojure and Datastar 🚀"]
    (let [[[board version]] (d/q db '{select [data v]
                                      from   chunk
                                      where  [= id 1]
                                      limit  1}
                              (fn [stmt] [(d/blob stmt 0) (d/int stmt 1)]))]
      (Board board version))]])

(defn migrations [db]
  ;; Note: all this code must be idempotent
  ;; Create tables
  (println "Running migrations...")
  (d/q db
    ["CREATE TABLE IF NOT EXISTS chunk(id INTEGER PRIMARY KEY, v INTEGER, data BLOB)"])
  (when (= 0 (first
               (d/q db '{select [[[count *]]] from chunk limit 1}
                 (fn [stmt] (d/int stmt 0)))))
    (d/q db
      '{insert-into chunk
        values      [{id 1 v 1 data ?board}]}
      {:board (game/empty-board board-size board-size)})))

(defn batch-fn [{:keys [db]} thunks]
  (let [[board] (d/q db '{select [data]
                          from   chunk
                          where  [= id 1]
                          limit  1}
                  (fn [stmt] (d/blob stmt 0)))]
    (run! (fn [thunk] (thunk db board)) thunks)
    (d/q db '{update chunk
              set    {id   1
                      v    [+ v 1]
                      data ?new-chunk}}
      {:new-chunk (game/next-gen-board
                    {:board    board
                     :max-rows board-size
                     :max-cols board-size})})))

(defonce app_ (atom nil))

(defn start-app! [& {:keys [mode]}]
  (reset! app_
    (h/start-app
      {:batch-fn #'batch-fn
       :tick-ms  100
       :email    (h/env :email)
       :domain   (h/env :domain)
       :mode     mode
       :dbs      {:db {:name          "gol.db"
                       :migrations    migrations
                       :pragma-writer {:cache_size 15625}
                       :pragma
                       {:cache_size 2000
                        :page_size  4096
                        ;; 1GB
                        :mmap_size  1000000000}}}})))

(defn -main [& _]
  (start-app! :mode :prod))

(comment
  (do (start-app! :mode :dev) nil)
  ;; (clojure.java.browse/browse-url "http://localhost:8080/")

  ;; stop server
  ((@app_ :stop!))

  ,)

(comment
  (require '[clj-async-profiler.core :as prof])

  (prof/start {:event :alloc})
  (prof/start)
  (prof/stop)
  (prof/serve-ui 7777)
  ;; (clojure.java.browse/browse-url "http://localhost:7777/")
  )




 
