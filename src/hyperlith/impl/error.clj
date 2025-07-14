(ns hyperlith.impl.error)

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
    (or (try-on-error req (handler req)) {:status 500})))

(defn default-on-error [{:keys [cause trace type] :as _error}]
  (println "")
  (println type)
  (println cause)
  (println "")
  (run! println trace)
  (flush))
