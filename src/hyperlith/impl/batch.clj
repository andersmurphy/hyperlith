(ns hyperlith.impl.batch
  (:require [clojure.core.async :as a]
            [hyperlith.impl.util :as util]))

(defn batch!
  "Wraps side-effecting function in a queue and batch mechanism. The batch is
  run every X ms when not empty and/or if it reaches it's max size. Function
  must take a vector of items."
  [effect-fn & {:keys [run-every-ms max-size after-run]
                :or   {run-every-ms 100
                       max-size     1000}}]
  (let [<in (a/chan 1000)] ;; potentially pass this channel in
    (util/thread
      (loop [<t    (a/timeout run-every-ms)
             batch []]
        (let [[v p] (a/alts!! [<t <in] :priority true)]
          (cond
            ;; Reset timer
            (and (= p <t) (= (count batch) 0))
            (recur (a/timeout run-every-ms) batch)

            ;; Run batch
            (or (= p <t)
              (and (= p <in) (>= (count batch) max-size)))
            (do (effect-fn (apply concat batch))
                (after-run)
                (recur (a/timeout run-every-ms) []))

            ;; Add to batch
            (= p <in)
            (recur <t (conj batch v))

            ;; if upstream is close run final batch and stop
            :else
            (do (effect-fn (apply concat batch))
                (after-run))))))
    (fn [items]
      (a/>!! <in items))))



