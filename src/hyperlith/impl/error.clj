(ns hyperlith.impl.error 
  (:require
   [hyperlith.impl.crypto :as crypto]
   [hyperlith.impl.util :as u]
   [clojure.main :as main]
   [clojure.string :as str]))

(defonce on-error_ (atom nil))

(def demunge-csl-xf
  (map (fn [stack-element-data]
         (update stack-element-data 0 (comp main/demunge str)))))

(def demunge-anonymous-functions-xf
  (map (fn [stack-element-data]
         (update stack-element-data 0 str/replace #"(/[^/]+)--\d+" "$1"))))

(def ignored-cls-re
  (re-pattern
    (str "^("
      (str/join "|"
        [;; A lot of this stuff is filler
         "clojure.lang"
         "clojure.main"
         "clojure.core.server"
         "clojure.core/eval"
         "clojure.core/binding-conveyor-fn"
         "java.util.concurrent.FutureTask"
         "java.util.concurrent.ThreadPoolExecutor"
         "java.util.concurrent.ThreadPoolExecutor/Worker"
         "java.lang.Thread"])
      ").*")))

(def remove-ignored-cls-xf
  ;; We don't care about var indirection
  (remove (fn [[cls _ _ _]] (re-find ignored-cls-re cls))))

(def not-hyperlith-cls-xf
  ;; trim error trace to users space helps keep trace short
  (take-while (fn [[cls _ _ _]] (not (str/starts-with? cls "hyperlith")))))

(defn add-error-id
  [error]
  (assoc error :id (crypto/digest (select-keys error [:trace :type]))))

(defn log-error [req t]
  (@on-error_
   {;; req is under own key as it can contain data you don't want to log.
    :req   (dissoc req :async-channel :websocket?)
    :error (let [m (Throwable->map t)]
             (-> m
               (update :cause str/replace #"\"" "'")
               (update :trace (fn [trace]
                                (into []
                                  (comp demunge-csl-xf
                                    not-hyperlith-cls-xf
                                    remove-ignored-cls-xf
                                    demunge-anonymous-functions-xf
                                    ;; This shrinks the trace to the most
                                    ;; relevant line
                                    (u/dedupe-with first)
                                    ;; max number of lines
                                    (take 15))
                                  trace)))
               (assoc :type (-> m :via first :type str))
               add-error-id))}))

(defmacro try-log [data & body]
  `(try
     ~@body
     (catch Throwable ~'t
       (log-error ~data ~'t)
       ;; Return nil when there is an error
       nil)))

(defn wrap-error [handler]
  (fn [req]
    (or (try-log req (handler req)) {:status 400})))
