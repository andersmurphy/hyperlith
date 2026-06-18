#!/usr/bin/env bb

;; run with: bb load_test_script.clj
(require
  '[babashka.http-client :as http]
  '[cheshire.core :as json])

(def client
  (http/client
    (assoc-in http/default-client-opts [:ssl-context :insecure] true)))

(let [url    "https://localhost:3030/t_rqnpSL_NvK8EJhoBwkc6TNJ4VsLi1Fs"
      cookie "__Host-sid=vtwl34jCOZDoGVIORWfiCBKg0U0"]
  (dotimes [_ 1000]
    (http/post url
      {:client  client
       :headers {"Cookie"          cookie
                 "Accept-Encoding" "br"
                 "sec-fetch-site"  "same-origin"
                 "Content-Type"    "application/json"}
       :body    (json/encode
                  {"tabid"    "7dc673ca"
                   "targetid" (str (rand-int 255))
                   "parentid" (str 0)})})
    (Thread/sleep 50)))
