(ns hyperlith.impl.cache
  "Simple unbounded cache that avoids thundering heard."
  (:refer-clojure :exclude [get])
  (:import (java.util.concurrent ConcurrentHashMap)))

(defn new []
  (ConcurrentHashMap/new))

(defn get [^ConcurrentHashMap cache k f]
  (ConcurrentHashMap/.computeIfAbsent cache k f))

(defn clear! [^ConcurrentHashMap cache]
  (ConcurrentHashMap/.clear cache))
