(ns hyperlith.impl.error 
  (:require [clojure.string :as str]))

(defonce on-error_ (atom nil))

(defn log-error [t]
  (-> (Throwable->map t)
    (dissoc :via)
    (update :trace
      (fn [trace]
        (->> trace
          (take-while
            (fn [[cls _ _ _]]
              ;; trim error trace to users space helps keep trace short
              ;; Note: If starts-with "hyperlith" is too aggressive
              ;; "hyperlith.impl.router" still would trim a lot of the noise.
              (not (str/starts-with? (str cls) "hyperlith"))))
          vec)))
    (@on-error_)))

(defmacro try-log [& body]
  `(try
     ~@body
     (catch Throwable ~'t
       (log-error ~'t))))

(defn wrap-error [handler]
  (fn [req]
    (or (try-log (handler req)) {:status 400})))
