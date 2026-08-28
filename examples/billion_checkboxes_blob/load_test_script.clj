#!/usr/bin/env bb

;; run with: bb load_test_script.clj
(require
  '[babashka.http-client :as http]
  '[cheshire.core :as json])

(def client
  (http/client
    (-> (assoc-in http/default-client-opts [:ssl-context :insecure] true)
      (assoc :version :http1.1))))

(def headers
  {"Cookie"          "__Host-sid=vtwl34jCOZDoGVIORWfiCBKg0U0"
   "Accept-Encoding" "zstd"
   "sec-fetch-site"  "same-origin"})

(def latency-threshold-ms 200)
(def stats (atom {:count 0 :max 0 :threshold-breaches 0}))

(defn record-latency! [ms]
  (let [breach? (> ms latency-threshold-ms)]
    (swap! stats
      (fn [s]
        (-> s
          (update :count inc)
          (update :max max ms)
          (update :threshold-breaches #(if breach? (inc %) %)))))))

(defn views [url n]
  (dotimes [_ n]
    (Thread/startVirtualThread
      (fn []
        (let [resp (http/post url
                     {:client  client
                      :headers headers
                      :as      :stream})]
          (with-open [in (:body resp)]
            (loop [last-time (System/currentTimeMillis)
                   buf       (byte-array 4096)]
              (let [avail (.available in)]
                (if (zero? avail)
                  (do (Thread/sleep 10)
                      (recur last-time buf))
                  (let [read (.read in buf 0 (min avail 4096))]
                    (if (neg? read)
                      (println "Stream closed")
                      (let [now    (System/currentTimeMillis)
                            gap-ms (- now last-time)]
                        (record-latency! gap-ms)
                        (Thread/sleep 10)
                        (recur now buf)))))))))))))

(defn actions [url n]
  (Thread/startVirtualThread
    (fn []
      (dotimes [_ n]
        (Thread/startVirtualThread
          (fn []
            (http/post url
              {:client  client
               :headers (assoc headers "Content-Type" "application/json")
               :body    (json/encode
                          {"tabid"    "7dc673ca"
                           "targetid" (str (rand-int 255))
                           "parentid" (str 0)})})))
        (Thread/sleep 5)))))

(let [url "http://localhost:8080"]
  (println "Running against..." url)
  (views (str url "/?u=") 2000)
  (actions (str url"/t_rqnpSL_NvK8EJhoBwkc6TNJ4VsLi1Fs")
    2000)
  (Thread/sleep 20000)
  (println @stats))
