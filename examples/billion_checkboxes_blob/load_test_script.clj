#!/usr/bin/env bb

;; run with: bb load_test_script.clj
(require
  '[babashka.http-client :as http]
  '[cheshire.core :as json]
  '[clojure.edn :as edn])

(def client
  (http/client
    (-> (assoc-in http/default-client-opts [:ssl-context :insecure] true)
      (assoc :version :http1.1))))

(defn gen-users [n]
  (mapv (fn [i] (str "__Host-sid=" "test-user-" i))
    (range 0 n)))

(def tiles-with-data (edn/read-string (slurp "load-test-data.edn")))

(let [positions (atom tiles-with-data)]
  (defn next-xy []
    (peek (swap! positions pop))))

(def headers
  {"Accept-Encoding" "zstd"
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

(defn views [users url]
  (dotimes [i (count users)]
    (Thread/startVirtualThread
      (fn []
        (let [resp (http/post url
                     {:client  client
                      :headers (assoc headers
                                 "Cookie" (users i)
                                 "Content-Type" "application/json")
                      :body    (json/encode {"tabid" "7dc673ca"})
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

(defn actions [users url data-generator]
  (Thread/startVirtualThread
    (fn []
      (dotimes [i (count users)]
        (Thread/startVirtualThread
          (fn []
            (http/post url
              {:client  client
               :headers (assoc headers
                          "Content-Type" "application/json"
                          "Cookie" (users i))
               :body    (json/encode (data-generator))})))
        (Thread/sleep 5)))))

(let [url   "http://localhost:8080"
      users (gen-users 2000)]
  (println "Running against..." url)
  (views users (str url "/?u="))
  ;; (actions users
  ;;   (str url "/t_rqnpSL_NvK8EJhoBwkc6TNJ4VsLi1Fs")
  ;;   (fn []
  ;;     {"tabid"    "7dc673ca"
  ;;      "targetid" (str (rand-int 255))
  ;;      "parentid" (str 0)}))
  (actions users
    (str url "/wp4BXB8NeCPWmq9C5rgU7-zyDP57-yYBk")
    (fn []
      (let [[x y] (next-xy)]
        {"tabid"  "7dc673ca"
         "view-x" x
         "view-y" y
         })))
  (Thread/sleep 20000)
  (println @stats))
