(ns hyperlith.impl.util
  (:refer-clojure :exclude [merge])
  (:require [clojure.java.io :as io])
  (:import (java.io InputStream)))

(defn assoc-conj
  "Associate a key with a value in a map. If the key already exists in the map,
  a vector of values is associated with the key."
  [map key val]
  (assoc map key
    (if-let [cur (get map key)]
      (if (vector? cur)
        (conj cur val)
        [cur val])
      val)))

(defn try-parse-long
  ([x] (try-parse-long x nil))
  ([x default]
   (or (try (parse-long x) (catch Throwable _ default)) default)))

(defn merge
  "Faster merge."
  [m1 m2]
  (persistent! (reduce-kv assoc! (transient (or m1 {})) m2)))

(defmacro thread
  "Starts a virtual thread. Conveys bindings."
  [& body]
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

(defn modulo-pick
  "Given a coll and a value x. Returns a random value from coll.
  Always returns the same value for a given coll and value."
  [coll x]
  (-> (hash x) (mod (count coll)) coll))
