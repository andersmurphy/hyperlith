(ns hyperlith.impl.engine
  (:require [hyperlith.impl.util :as util]
            [sqlite4clj.core :as d]
            [sqlite4clj.litestream :as l]
            [org.httpkit.server :as hk]
            [hyperlith.impl.headers :refer [default-headers]]
            [hyperlith.impl.brotli :as br])
  (:import (java.util.concurrent BlockingQueue ArrayBlockingQueue
             Executors ExecutorService)
           (java.util ArrayList)))

(defmacro on-pool! [pool & body]
  `(ExecutorService/.submit ~pool ^Callable
     (fn [] ~@body)))

(def connections_ (atom {}))

(defn send! [ch event]
  (hk/send! ch {:status  200
                :headers (assoc default-headers
                           "Content-Type"  "text/event-stream"
                           "Cache-Control" "no-store"
                           "Content-Encoding" "br")
                :body    event}
    false))

(defn add-connection! [ch render-fn]
  (swap! connections_ assoc
    ch (let [out (br/byte-array-out-stream)
             br  (br/compress-out-stream out
                   :window-size 24)
             last-view-hash_ (atom nil)]
         (fn engine-connection [db]
           (try
             (if (hk/open? ch)
               (let [view (render-fn db)
                     new-view-hash (hash view)
                     render? (not= @last-view-hash_ new-view-hash)]
                 (when render?
                   (reset! last-view-hash_ new-view-hash)
                   (->> view
                     (br/compress-stream out br)
                     (send! ch))))
               ;; Clean up connection
               (do (swap! connections_ dissoc ch)
                   (.close br)))
             (catch Throwable _
               (.close br)))))))

(defn start!
  [db-name {:keys [migrations litestream pragma cache-write-fn]
            :or   {cache-write-fn (fn [_db _cache] nil)}}]
  (let [core-count (Runtime/.availableProcessors (Runtime/getRuntime))
        _          (when litestream
                     (l/restore-then-replicate! db-name litestream))
        {:keys [writer reader]}
        (d/init-db! db-name
          {:pool-size core-count
           :pragma    pragma})
        _          (migrations writer)
        cpu-pool   (Executors/newFixedThreadPool core-count)
        |queue     (ArrayBlockingQueue/new 10000)
        batch      (ArrayList/new 10000)]
    (util/virtual-thread ;; Virtual thread for cheap blocking
      (while true
        (if (= (count |queue) 0)
          (Thread/sleep 1) ;; let other v threads run
          (do
            ;; Adaptive batching of actions
            ;; drainTo is not blocking
            (BlockingQueue/.drainTo |queue batch)
            @(on-pool! cpu-pool
               (d/with-write-tx [db writer]
                 (let [cache (atom {})]
                   (run! (fn [thunk] (thunk db cache)) batch)
                   (cache-write-fn db cache))))
            ;; Update views
            (let [conns @connections_]
              (->> conns
                (into []
                  (comp
                    (map (fn [[_ v]] v))
                    (partition-all (int (/ (count conns) core-count)))
                    (map (fn [conn-batch]
                           ;; Use the same connection per thread batch
                           (on-pool! cpu-pool
                             (d/with-conn [db reader]
                               (run! (fn [conn] (conn db)) conn-batch)))))))
                (run! deref)))
            (ArrayList/.clear batch)))))
    {:tx!    (fn [thunk] (BlockingQueue/.put |queue thunk))
     :writer writer
     :reader reader
     :stop!  nil}))

;; TODO: max fps (to protect the browser)?
;; TODO: refactor examples
;; TODO: update readme
;; TODO: error handling

(comment
  (count @connections_))
