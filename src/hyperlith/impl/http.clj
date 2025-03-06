(ns hyperlith.impl.http
  (:require [org.httpkit.client :as http]
            [hyperlith.impl.json :as json]
            [hyperlith.impl.gzip :as gzip]
            [clojure.string :as str])
  (:import [org.httpkit DynamicBytes]
           [org.httpkit.client IFilter]))

(defn has-json-body? [resp]
  (and (:body resp)
    (-> resp :headers :content-type (= "application/json"))))

(defn wrap-json-response [method]
  (fn [url request]
    (let [req @(method url request)]
      (cond-> req
        (has-json-body? req) (update :body json/try-json->edn)))))

(def get! (wrap-json-response #_:clj-kondo/ignore http/get))

(def post! (wrap-json-response #_:clj-kondo/ignore http/post))

(defn byte-capture-filter
  "Captures bytes and returns the last capture of the whole buffer."
  [capture]
  (reify IFilter
    (^boolean accept [_this ^DynamicBytes bytes]
     ;; As DynamicBytes accumulates we only care about the final result
     ;; before close or timeout.
     (reset! capture (.bytes bytes))
     true)
    (^boolean accept [_this ^java.util.Map _m]
     true)))

(defn split-events [s]
  (str/split s #"\n\n\n"))

(defn parse-sse-events [events]
  (mapv
    (fn [event]
      (reduce (fn [acc line]
                (let [[k v] (update (str/split line #": " 2) 0 keyword)]
                  (if (= :data k)
                    (update acc :data conj v)
                    (assoc acc k v))))
        {:data []}
        (str/split-lines event)))
    (str/split events #"\n\n\n")))

(defn wrap-gzipped-sse-response [method]
  (fn [url request]
    (let [capture (atom nil)]
      #_:clj-kondo/ignore
      @(method
         url
         (assoc request
           ;; We have to do gzip ourselves otherwise the filter will wait
           ;; for the stream to complete which could be never.
           :as :none
           :filter  (byte-capture-filter capture)))
      (-> @capture gzip/gunzip parse-sse-events))))

(def sse-get!
  (wrap-gzipped-sse-response #_:clj-kondo/ignore http/get))

(def sse-post!
  (wrap-gzipped-sse-response #_:clj-kondo/ignore http/post))

(comment
  ;; TODO: stream gunzip?
  ;; TODO: return a channel or atom of SSE events

  (sse-post!
    "http://localhost:8080/"
    {:timeout 1
     :headers
     {"Cookie"       "__Host-sid=5SNfeDa90PhXl0expOLFGdjtrpY; __Host-csrf=3UsG62ic9wLsg9EVQhGupw"
      "Content-Type" "application/json"}
     :body    "{\"csrf\":\"3UsG62ic9wLsg9EVQhGupw\"}"})

  )
