(ns hyperlith.impl.error 
  (:require [clojure.string :as str]))

(defonce on-error_ (atom nil))

(defn log-error [req t]
  (@on-error_
   {;; req is under own key as it can contain data you don't want to log.
    :req   (dissoc req :async-channel :websocket?)
    :error (-> (Throwable->map t)
             (dissoc :via)
             (update :trace
               (fn [trace]
                 (->> trace
                   (take-while
                     (fn [[cls _ _ _]]
                       ;; trim error trace to users space helps keep trace short
                       (not (str/starts-with? (str cls) "hyperlith"))))
                   vec))))}))

(defmacro try-log [data & body]
  `(try
     ~@body
     (catch Throwable ~'t
       (log-error ~data ~'t))))

(defn wrap-error [handler]
  (fn [req]
    (or (try-log req (handler req))  {:status 400})))
