(ns hyperlith.impl.cpu-pool
  (:import (java.util.concurrent Executors ExecutorService)))

(defonce pool
  (Executors/newFixedThreadPool
    (Runtime/.availableProcessors (Runtime/getRuntime))))

(defmacro on-cpu-pool [& body]
  `(deref (ExecutorService/.submit pool ^Callable (fn [] ~@body))))
