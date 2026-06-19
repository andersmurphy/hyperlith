#!/usr/bin/env bb

;; run with: bb load_test_script.clj
(require
  '[babashka.http-client :as http]
  '[cheshire.core :as json])

(def client
  (http/client
    (assoc-in http/default-client-opts [:ssl-context :insecure] true)))

(def headers
  {"Cookie"          "__Host-sid=vtwl34jCOZDoGVIORWfiCBKg0U0"
   "Accept-Encoding" "br"
   "sec-fetch-site"  "same-origin"})

(let [url "https://localhost:3030/?u="]
  (dotimes [_ 100]
    (Thread/startVirtualThread
      (fn []
        (try
          (let [resp (http/get url
                       {:client  client
                        :headers headers
                        :as      :stream})]
            (with-open [in (:body resp)]
              (while true
                (.skip in Long/MAX_VALUE)
                (Thread/sleep 10))))
          (catch Exception _)
          (println "Stream closed"))))))

(let [url "https://localhost:3030/t_rqnpSL_NvK8EJhoBwkc6TNJ4VsLi1Fs"]
  (dotimes [_ 10000]
    (Thread/startVirtualThread
      (fn []
        (http/post url
          {:client  client
           :headers (assoc headers "Content-Type" "application/json")
           :body    (json/encode
                      {"tabid"    "7dc673ca"
                       "targetid" (str (rand-int 255))
                       "parentid" (str 0)})})))
    (Thread/sleep 50)))
