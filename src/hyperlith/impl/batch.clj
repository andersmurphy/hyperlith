(ns hyperlith.impl.batch
  (:require [clojure.core.async :as a]
            [hyperlith.impl.util :as util]
            [hyperlith.impl.error :as er]))

(defn batch!
  "Wraps side-effecting function in a queue and batch mechanism. The
  batch is run every X ms when not empty and/or if it reaches it's max
  size. Function must take a vector of items. Supports back pressure."
  [effect-fn & {:keys [run-every-ms max-size]
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
            (let [;; Timer is started before task to prevent task duration
                  ;; from affecting interval (unless it exceeds interval).
                  <new-t (a/timeout run-every-ms)]
              (er/try-on-error (effect-fn batch))
              (recur <new-t []))

            ;; Add to batch
            (= p <in)
            (recur <t (conj batch v))

            ;; if upstream is close run final batch and stop
            :else
            (er/try-on-error {} (effect-fn (apply concat batch)))))))
    (fn [items]
      (a/>!! <in items))))
