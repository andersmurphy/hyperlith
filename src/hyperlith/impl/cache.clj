(ns hyperlith.impl.cache)

(defonce ^:private cache_ (atom {}))

(defn- assoc-if-missing [m k v]
  (if-not (m k) (assoc m k v) m))

(defn cache [f]
  ;; Note: cache has no upper bound and is only cleared when a refresh
  ;; event is fire.
  (fn [& args]
    (let [k         [f args]
          ;; By delaying the value we make it lazy
          ;; then it gets evaluated on first read.
          ;; This prevents stampedes.
          new-value (delay (apply f args))]
      @((swap! cache_ assoc-if-missing k new-value) k))))

(defn invalidate-cache! []
  (reset! cache_ {}))
