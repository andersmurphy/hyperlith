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

(defmacro run-batch-on-pool! [pool f batch]
  `(ExecutorService/.submit ~pool ^Callable
     (fn [] (run! ~f ~batch))))

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
                   :window-size 18)]
         (fn engine-connection [db]
           (try
             (if (hk/open? ch)
               (some->> (render-fn db)
                 (br/compress-stream out br)
                 (send! ch))
               ;; Clean up connection
               (do (swap! connections_ dissoc ch)
                   (.close out)
                   (.close br)))
             (catch Throwable _
               (.close out)
               (.close br)))))))

(defn start!
  [db-name {:keys [migrations litestream pragma]}]
  (let [core-count (Runtime/.availableProcessors (Runtime/getRuntime))
        _          (when litestream
                     (l/restore-then-replicate! db-name litestream))
        {:keys [writer reader]}
        (d/init-db! db-name
          {:pool-size core-count
           :pragma    pragma})
        _          (migrations writer)
        cpu-pool   (Executors/newFixedThreadPool core-count)
        |queue     (ArrayBlockingQueue/new 20000)
        batch      (ArrayList/new 20000)]
    (util/virtual-thread ;; Virtual thread for cheap blocking
      (while true
        ;; Adaptive batching of actions        
        (println "=======")
        (time
          (do
            (ArrayList/.clear batch)
            (ArrayList/.add batch (BlockingQueue/.take |queue))
            (BlockingQueue/.drainTo |queue batch)))
        (time
          (d/with-write-tx [tx writer]
            @(run-batch-on-pool! cpu-pool
               (fn [thunk] (thunk tx))
               batch)))
        ;; Update views
        (time
          (let [conns @connections_]
          (->> conns
            (into []
              (comp
                (map (fn [[_ v]] v))
                (partition-all (int (/ (count conns) core-count)))
                (map (fn [thunk-batch]
                       ;; Use the same connection per batch
                       (d/with-conn [tx reader]
                         (run-batch-on-pool! cpu-pool
                           (fn [thunk] (thunk tx)) thunk-batch))))))
            (run! deref))))))
    {:tx!   (fn [thunk]
              (BlockingQueue/.add |queue thunk))
     :stop! nil}))
  
;; TODO; fix weird check behaviour
;; TODO: max fps (to protect the browser)
;; TODO: only send on change
;; TODO: connection close batch?
;; TODO: refactor examples
;; TODO: update readme
;; TODO: benchmark new vs old
;; TODO: profile new vs old
;; TODO: pray
;; TODO: check errors
;; TODO: error handling

(comment
  (count @connections_))
