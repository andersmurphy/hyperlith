(ns hyperlith.impl.util
  (:refer-clojure :exclude [merge])
  (:require [clojure.java.io :as io])
  (:import (java.io InputStream)))

(defn merge
  "Faster merge."
  [m1 m2]
  (persistent! (reduce-kv assoc! (transient (or m1 {})) m2)))

(defmacro thread [& body]
  `(Thread/startVirtualThread
     (bound-fn* ;; binding conveyance
       (fn [] ~@body))))

(defmacro while-some
  {:clj-kondo/lint-as 'clojure.core/let}
  [bindings & body]
  `(loop []
     (when-some ~bindings
       ~@body
       (recur))))

(defn assoc-if-missing [m k v]
  (if-not (m k) (assoc m k v) m))

(defn assoc-in-if-missing [m ks v]
  (if-not (get-in m ks) (assoc-in m ks v) m))

(defn resource->bytes [resource]
  (-> resource io/input-stream InputStream/.readAllBytes))

(defmacro load-resource
  "Fails at compile time if resource doesn't exists."
  [path]
  (let [res (io/resource path)]
    (assert res (str path " not found."))
    `(resource->bytes (io/resource ~path))))

(defn qualify-keys
  "Adds qualifier to key. Overwrites existing qualifier. Is idempotent."
  [m ns]
  (update-keys m (fn [k] (keyword (name ns) (name k)))))

(defn dedupe-with
  ([f]
   (fn [rf]
     (let [pv (volatile! ::none)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [prior   @pv
                f-input (f input)]
            (vreset! pv f-input)
            (if (= prior f-input)
              result
              (rf result input))))))))
  ([f coll] (sequence (dedupe-with f) coll)))
