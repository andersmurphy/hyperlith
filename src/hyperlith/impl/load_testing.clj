(ns hyperlith.impl.load-testing
  (:require [clojure.string :as str]
            [hyperlith.impl.brotli :as br]
            [org.httpkit.client :as http])
  (:import [java.util Arrays]
           [org.httpkit DynamicBytes]
           [org.httpkit.client IFilter]))

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

(defn byte-capture-filter
  "Captures bytes and returns the last capture of the whole buffer."
  [capture]
  (reify IFilter
    (^boolean accept [_this ^DynamicBytes bytes]
     ;; As DynamicBytes accumulates we only care about the final result
     ;; before close or timeout.
     (let [prev-length (:length @capture)
           next-length (int (.length bytes))]
       (swap! capture
         #(-> (assoc % :length next-length)
            ;; Currently we only decompress these values at the end
            ;; in theory this could be replaced with a streaming
            ;; decompression.
            (update :events into (Arrays/copyOfRange
                                   ^byte/1 (.get bytes)
                                   ^int prev-length
                                   ^int next-length)))))
     true)
    (^boolean accept [_this ^java.util.Map m]
     (swap! capture assoc :headers m)
     true)))

(defn wrap-capture-sse-response [method]
  (fn [url request]
    (let [capture (atom {:length 0
                         :events   []})]
      #_:clj-kondo/ignore
      (method
        url
        (assoc request
          ;; We have to do decompress ourselves otherwise the filter will wait
          ;; for the stream to complete which could be never.
          :as :none
          :filter  (byte-capture-filter capture)))
      capture)))

(def capture-sse-get!
  (wrap-capture-sse-response #_:clj-kondo/ignore http/get))

(def capture-sse-post!
  (wrap-capture-sse-response #_:clj-kondo/ignore http/post))

(defn parse-captured-response [capture]
  (cond-> capture
    (= (get-in capture [:headers "content-encoding"]) "br")
    (update :events (comp br/decompress-stream byte-array))
    :always (update :events parse-sse-events)))

(comment
  (def capture
    (capture-sse-post!
      "http://localhost:8080/"
      {:timeout 1
       :headers
       {"accept-encoding" "br"
        "cookie"       "__Host-sid=5SNfeDa90PhXl0expOLFGdjtrpY; __Host-csrf=3UsG62ic9wLsg9EVQhGupw"
        "content-type" "application/json"}
       :body    "{\"csrf\":\"3UsG62ic9wLsg9EVQhGupw\"}"}))

  (-> @capture parse-captured-response)

  )
