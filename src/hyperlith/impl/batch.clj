(ns hyperlith.impl.batch
  (:require [hyperlith.impl.util :as util]
            [hyperlith.impl.error :as er]
            [hyperlith.impl.cpu-pool :as cp]))

(defn batch!
  "Wraps side-effecting function in a queue and batch mechanism. The
  batch is run every X ms when not empty. Function must take a vector of items. Supports back pressure."
  [effect-fn & {:keys [run-every-ms]
                :or   {run-every-ms 100}}]
  (let [batch_ (atom [])]
    (util/thread ;; this is v thread
      (loop []
        (if (= (count @batch_) 0)
          ;; Reset timer
          (do (Thread/sleep ^long run-every-ms)
              (recur))
          ;; Run batch
          (let [;; Timer is started before task to prevent task duration
                ;; from affecting interval (unless it exceeds interval).
                start     (System/currentTimeMillis)
                [batch _] (reset-vals! batch_ [])]
            (cp/on-cpu-pool ;; CPU work on real threads
              (er/try-on-error (effect-fn batch)))
            (let [delta (- run-every-ms (- (System/currentTimeMillis) start))]
              (when (> delta 0)
                (Thread/sleep ^long delta)))
            (recur)))))
    (fn [items]
      (swap! batch_ conj items))))


(comment
  (def batch-v1
    (batch! (fn [_] (Thread/sleep 10))
      {:run-every-ms 100}))

  (user/bench
    (->> (mapv (fn [_] (future (batch-v1 1)))
           (range 0 2000))
         (run! (fn [x] @x)))))
