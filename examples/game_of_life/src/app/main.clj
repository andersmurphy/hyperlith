(ns app.main
  (:gen-class)
  (:require [clojure.pprint :as pprint]
            [hyperlith.core :as h]
            [app.game :as game]
            [hyperlith.impl.util :as u]))

(def board-size 40)

(def css
  (let [black :black]
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

       [:.b
        {:background            black
         :gap                   :2px
         :padding               :2px
         :margin-inline         :auto
         :max-width             "min(100%, 30rem)"
         :display               :grid
         :aspect-ratio          "1/1"
         :grid-template-rows    (str "repeat(" board-size ", 1fr)")
         :grid-template-columns (str "repeat(" board-size ", 1fr)")}]

       [:.t
        {:background :white
         :transition "background 0.4s ease"}]])))

(def board-state
  (h/cache
    (fn [db]
      (map-indexed
        (fn [id full]
          (h/html
            [:div
             {:class         (if full "" "t")
              :id            id
              :data-on-click (format "@post('/tap?id=%s')" id)}]))
        (:board @db)))))

(defn render-home [{:keys [db] :as _req}]
    (h/html
      [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}]
      [:main#morph.main
       [:h1 "Game of Life (multiplayer)"]
       [:p "Built with ❤️ using "
        [:a {:href "https://clojure.org/"} "Clojure"]
        " and "
        [:a {:href "https://data-star.dev"} "Datastar"]
        "🚀"]
       [:p "Source code can be found "
        [:a {:href "https://github.com/andersmurphy/hyperlith/blob/master/examples/game_of_life/src/app/main.clj"} "here"]]
       [:p [:i nil (str "Connected players: " (:connected-counter @db))]]
       [:div
        [:div.b nil
         (board-state db)]]]))

(defn fill-cell [board id]
  (if ;; crude overflow check
      (<= 0 id (dec (* board-size board-size)))
    (assoc board id true)
    board))

(defn fill-cross [db id]
  (-> db
    (update :board fill-cell (- id board-size))
    (update :board fill-cell (- id 1))
    (update :board fill-cell    id)
    (update :board fill-cell (+ id 1))
    (update :board fill-cell (+ id board-size))))

(defn action-tap-cell [{:keys [_sid db] {:strs [id]} :query-params}]
  (swap! db fill-cross (parse-long id)))

(def default-shim-handler
  (h/shim-handler
    (h/html
      [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}]
      [:title nil "Game of Life"]
      [:meta {:content "Conway's Game of Life" :name "description"}])))

(defn next-gen-board [current-board]
  (game/next-gen-board
    {:board    current-board
     :max-rows board-size
     :max-cols board-size}))

(defn next-generation! [db]
  (swap! db update :board next-gen-board))

(defn start-game! [db]
  (let [running_ (atom true)]
    (u/thread
      (while @running_
        (Thread/sleep 200) ;; 5 fps
        (next-generation! db)))
    (fn stop-game! [] (reset! running_ false))))

(def router
  (h/router
    {[:get (css :path)] (css :handler)
     [:get  "/"]        default-shim-handler
     [:post "/"]        (h/render-handler #'render-home
                          :on-open
                          (fn [{:keys [db]}]
                            (swap! db update :connected-counter inc))
                          :on-close
                          (fn [{:keys [db]}]
                            (swap! db update :connected-counter dec)))
     [:post "/tap"]     (h/action-handler #'action-tap-cell)}))

(defn ctx-start []
  (let [db_ (atom {:board             (game/empty-board board-size board-size)
                   :connected-counter 0})]
    (add-watch db_ :refresh-on-change
      (fn [_ _ old-state new-state]
        ;; Only refresh if state has changed
        (when-not (= old-state new-state)
          (h/refresh-all!))))
    {:db        db_
     :game-stop (start-game! db_)}))

(defn -main [& _]
  (h/start-app
    {:router         #'router
     :max-refresh-ms 100
     :ctx-start      ctx-start
     :ctx-stop       (fn [{:keys [game-stop]}] (game-stop))
     :csrf-secret    (h/env :csrf-secret)
     :on-error       (fn [_ctx {:keys [req error]}]
                       (pprint/pprint req)
                       (pprint/pprint error))}))

;; Refresh app when you re-eval file
(h/refresh-all!)

(comment
  (-main)
  ;; (clojure.java.browse/browse-url "http://localhost:8080/")

  ;; stop server
  (((h/get-app) :stop))

  (def db (-> (h/get-app) :ctx :db))

  (->> @db :board (remove false?))

  
  ,)

(comment
  (game/next-gen-board
    {:board     (game/empty-board board-size board-size)
     :max-rows board-size
     :max-cols board-size}))
