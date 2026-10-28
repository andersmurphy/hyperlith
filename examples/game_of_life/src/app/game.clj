;; This code based of (with some modifications): https://github.com/kaepr/game-of-life-cljs/blob/80ff8a16804e03d35d056cc3c64f4d0be9ce301e/src/app/game.cljs#L1C1-L83C6
(ns app.game)

(def grid-config
  {:name      "Square"
   :neighbors [[-1 -1] [-1 0] [-1 1]
               [0 -1]  #_cell [0 1]
               [1 -1]  [1 0]  [1 1]]})

(def dead-cell (int 0))

(defn alive-cell [living-neighbors] (int (rand-nth living-neighbors)))

(defn alive? [cell] (not= cell (int 0)))

(defn coordinates->index [row col max-cols]
  (+ col (* row max-cols)))

(defn index->coordinates [idx max-cols]
  [(quot idx max-cols) (rem idx max-cols)])

(defn get-cell [^byte/1 board idx]
  (when (< -1 idx (count board))
    (aget board idx)))

(defn update-cell [^byte/1 board [row col] max-cols cell]
  (aset-byte ^byte/1 board (int (coordinates->index row col max-cols)) (int cell)))

(defn empty-board [max-rows max-cols]
  (byte-array (vec (repeat (* max-rows max-cols) dead-cell))))

(defn get-neighbors
  "Returns all neighbors using 1d based vector indexing."
  [neighbors [row col] max-rows max-cols]
  (let [valid? (fn [r c] (and (>= r 0) (>= c 0) (< r max-rows) (< c max-cols)))]
    (->> neighbors
      (map (fn [[dr dc]]
             (let [r (+ row dr)
                   c (+ col dc)]
               (when (valid? r c)
                 (coordinates->index r c max-cols)))))
      (filter some?))))

(defn cell-transition [cell neighbors-count living-neighbors]
  (if (or (and (alive? cell) (or (= neighbors-count 2) (= neighbors-count 3)))
        (and (not (alive? cell)) (= neighbors-count 3)))
    (alive-cell living-neighbors)
    dead-cell))

(defn next-gen-board [{:keys [board max-rows max-cols]}]
  (let [next-board (byte-array board)
        size       (* max-rows max-cols)]
    (dotimes [idx size]
      (let [coords           (index->coordinates idx max-cols)
            cell             (get-cell board idx)
            neighbors        (:neighbors grid-config)
            neighbor-cells   (get-neighbors neighbors coords max-rows max-cols)
            living-neighbors (filter alive? (map #(get-cell board %)
                                              neighbor-cells))
            neighbor-count   (count living-neighbors)
            new-cell         (cell-transition cell neighbor-count
                               living-neighbors)]
        (aset-byte ^byte/1 next-board (int idx) (int new-cell))))
    next-board))

(comment

  (empty-board 5 5)

  (next-gen-board {:board    (byte-array [0 0 0 0 0
                                          0 0 1 0 0
                                          0 1 1 1 0
                                          0 0 1 0 0
                                          0 0 0 0 0])
                   :max-rows 5
                   :max-cols 5}))


