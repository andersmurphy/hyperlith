(ns hyperlith.impl.util
  (:require [clojure.java.io :as io])
  (:import (java.io InputStream)))

(defmacro thread [& body]
  `(Thread/startVirtualThread
     (fn [] ~@body)))

(defmacro while-some
  {:clj-kondo/lint-as 'clojure.core/let}
  [bindings & body]
  `(loop []
     (when-some ~bindings
       ~@body
       (recur))))

(defmacro resource
  "Fails at compile time if resource doesn't exists."
  [path]
  (let [res (io/resource path)]
    (assert res (str path " not found."))
    `(io/resource ~path)))

(defn resource->bytes [resource]
  (-> resource io/input-stream InputStream/.readAllBytes))
