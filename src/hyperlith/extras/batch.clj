(ns hyperlith.extras.batch 
  (:import
   [java.util ArrayList]
   [java.util.concurrent LinkedBlockingQueue]))

(defn async-batcher-init!
  "Creates an async batching process that batches.

   A batch-fn operates on a batch of thunks (functions) that can be passed
   the db plus any other context (like a cache) and generally describes an
   atomic sequence of reads/writes.

   Example batch-fn function:

   (fn batch-fn [ctx batch]
     (with-write-tx [db (:writer ctx)]
       (run! (fn [thunk] (thunk db)) batch)))

  This can be used to increase write throughput by batching writes in
  a single transaction. As well as an entry point for coarse or fine grained
  subscriptions to changes.

  Returns a function tx! that lets you dispatch async batch writes. By default
  tx!."
  [ctx & {:keys [batch-fn batch-tick-ms] :or {batch-tick-ms 50}}]
  (assert (not (nil? batch-fn)))
  (let [queue (LinkedBlockingQueue/new)]
    (Thread/startVirtualThread
      (bound-fn* ;; binding conveyance
        (fn batch-thread []
          (while (not (Thread/interrupted))
            (let [next-tick (+ (System/currentTimeMillis) batch-tick-ms)
                  batch     (ArrayList/new)]
              (.drainTo queue batch)
              (when-not (.isEmpty batch)
                (try
                  (batch-fn ctx (seq batch))
                  (catch Exception e
                    (println "batch-fn error:" e))))
              (Thread/sleep ;; sleep 0 to let other tasks run
                (int (max 0 (- next-tick (System/currentTimeMillis))))))))))
    (fn tx! [thunk] (LinkedBlockingQueue/.offer queue thunk))))


