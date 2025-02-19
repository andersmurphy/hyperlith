(ns hyperlith.impl.cache
  (:require [hyperlith.impl.util :as util]))

(defonce ^:private cache_ (atom {}))

(defn cache [f]
  ;; Note: cache has no upper bound and is only cleared when a refresh
  ;; event is fire.
  (fn [& args]
    (let [k         [f args]
          ;; By delaying the value we make it lazy
          ;; then it gets evaluated on first read.
          ;; This prevents stampedes.
          new-value (delay (apply f args))]
      @((swap! cache_ util/assoc-if-missing k new-value) k))))

(defn invalidate-cache! []
  (reset! cache_ {}))
