(ns hyperlith.impl.engine
  (:require [sqlite4clj.core :as d]
            [sqlite4clj.litestream :as l]
            [sqlite4clj.batch :as b]
            [org.httpkit.server :as hk]
            [hyperlith.impl.headers :refer [default-headers]]
            [hyperlith.impl.brotli :as br])
  (:import (java.util.concurrent Executors ExecutorService)))

(defmacro on-pool! [pool & body]
  `(ExecutorService/.submit ~pool ^Callable
     (fn [] ~@body)))

(defn send! [ch event]
  (hk/send! ch {:status  200
                :headers (assoc default-headers
                           "Content-Type"  "text/event-stream"
                           "Cache-Control" "no-store"
                           "Content-Encoding" "br")
                :body    event}
    false))

(defn render-handler [{:keys [on-close on-open] :as _opts} render-fn]
  (fn handler [{:keys [tx! connections_] :as req}]
    (let [closed?_ (atom nil)]
      (hk/as-channel req
        {:on-open
         (fn hk-on-open [ch]
           (swap! connections_ assoc ch
             (let [out             (br/byte-array-out-stream)
                   br              (br/compress-out-stream out
                                     :window-size 22)
                   last-view-hash_ (atom nil)]
               (fn engine-connection [db]
                 (try
                   ;; http-kit sometimes re-uses connections resurrecting
                   ;; closed connection. So we use a more permanent flag.
                   (if-not @closed?_
                     (let [view          (render-fn (assoc req :db db))
                           new-view-hash (hash view)
                           render?       (not= @last-view-hash_
                                           new-view-hash)]
                       (when render?
                         (reset! last-view-hash_ new-view-hash)
                         (->> view
                           (br/compress-stream out br)
                           (send! ch))))
                     ;; Clean up connection
                     (do (swap! connections_ dissoc ch)
                         (.close br)))
                   (catch Throwable _
                     (.close br))))))
           (tx! (fn [& _] nil))
           (when on-open (on-open req)))
         :on-close (fn hk-on-close [_ _]
                     (reset! closed?_ true)
                     (when on-close (on-close req)))}))))

(defn start
  [{:keys [db-name migrations litestream pragma batch-fn]}]
  (assert (not (nil? batch-fn)))
  (let [running_     (atom true)
        connections_ (atom {})
        core-count   (Runtime/.availableProcessors (Runtime/getRuntime))
        _            (when litestream
                       (l/restore-then-replicate! db-name litestream))
        {:keys [writer reader] :as db}
        (d/init-db! db-name
          {:pool-size core-count
           :pragma    pragma})
        _            (migrations writer)
        cpu-pool     (Executors/newFixedThreadPool core-count)
        tx!
        (b/async-batcher-init! db
          {:max-batch-size  10000
           :return-promise? false
           :batch-fn
           (fn [db batch]
             @(on-pool! cpu-pool
                (d/with-write-tx [db db]
                  (batch-fn db batch)))
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
                 (run! deref))))})]
    [(fn []
       (reset! running_ false)
       (reset! connections_ {}))
     {:tx!          tx!
      :connections_ connections_
      :writer       writer
      :reader       reader}]))

;; TODO: handler sandboxing?
;; add-connection rather than connections
;; q instead of reader


;; TODO: max fps (to protect the browser)?
;; TODO: refactor examples
;; TODO: update readme
;; TODO: error handling
