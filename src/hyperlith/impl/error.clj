(ns hyperlith.impl.error 
  (:require
   [hyperlith.impl.crypto :as crypto]
   [clojure.main :as main]
   [clojure.string :as str]))

(defonce on-error_ (atom nil))

(def demunge-csl-xf
  (map (fn [stack-element-data]
         (update stack-element-data 0 (comp main/demunge str)))))

(def remove-ignored-cls-xf
  ;; We don't care about var indirection
  (remove (fn [[cls _ _ _]] (#{"clojure.lang.Var"} cls))))

(def not-hyperlith-cls-xf
  ;; trim error trace to users space helps keep trace short
  (take-while (fn [[cls _ _ _]] (not (str/starts-with? cls "hyperlith")))))

(defn add-error-id [{:keys [trace] :as error}]
  (assoc error :id (crypto/digest trace)))

(defn log-error [req t]
  (@on-error_
   {;; req is under own key as it can contain data you don't want to log.
    :req   (dissoc req :async-channel :websocket?)
    :error (-> (Throwable->map t)
             (dissoc :via)
             (update :trace (fn [trace]
                              (into []
                                (comp demunge-csl-xf
                                  remove-ignored-cls-xf
                                  not-hyperlith-cls-xf)
                                trace)))
             add-error-id)}))

(defmacro try-log [data & body]
  `(try
     ~@body
     (catch Throwable ~'t
       (log-error ~data ~'t))))

(defn wrap-error [handler]
  (fn [req]
    (or (try-log req (handler req)) {:status 400})))
