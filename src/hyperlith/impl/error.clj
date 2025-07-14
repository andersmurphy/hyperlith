(ns hyperlith.impl.error 
  (:require [clojure.pprint :as pprint]))

(defonce on-error_ (atom nil))

(defmacro try-on-error [& body]
  `(try
     ~@body
     (catch Throwable ~'t
       (@on-error_ ~'t)
       ;; Return nil when there is an error
       nil)))

(defn wrap-error [handler]
  (fn [req]
    (or (try-on-error (handler req)) {:status 500})))

(defn default-on-error [error]
  (pprint/pprint error)
  (flush))
