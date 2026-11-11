(ns hyperlith.impl.headers
  (:require [clojure.string :as str]
            [hyperlith.impl.util :as u]))

(def self "'self'")
(def none "'none'")
(def unsafe-inline "'unsafe-inline'")
(def unsafe-eval "'unsafe-eval'")

(def csp-data
  {:base-uri        [self]
   ;; Chrome and Safari do not allow redirects after submitting a form
   ;; unless the destination URL is listed in the form-action CSP rule,
   ;; even if it is a GET redirect that does not contain the original
   ;; form data.
   :form-action     [self]
   :default-src     [none]
   :media-src       [self "https: data:"]
   :script-src      [self unsafe-eval]
   :img-src         [self "https: data:"]
   :font-src        [self]
   :connect-src     [self]
   :style-src       [self unsafe-inline]
   :style-src-elem  [self unsafe-inline]
   :frame-ancestors [none]})

(defn csp-data->str [csp-data]
  (reduce
    (fn [acc [k v]] (str acc (name k) " " (str/join " " v) ";"))
    ""
    csp-data))

(def strict-transport
  "Forces https, including on subdomains. Prevents attacker from using
  compromised subdomain."
  "max-age=63072000;includeSubDomains;preload")

(def default-headers
  {"Content-Type"            "text/html; charset=utf-8"
   "Content-Security-Policy" (csp-data->str csp-data)
   "Referrer-Policy"         "no-referrer"
   "X-Content-Type-Options"  "nosniff"
   "X-Frame-Options"         "deny"
   "Cache-Control"           "no-cache, must-revalidate"})

(def sse-headers
  {"Strict-Transport-Security" strict-transport
   "Content-Type"              "text/event-stream"
   "Cache-Control"             "no-store"
   "X-Content-Type-Options"    "nosniff"
   "X-Frame-Options"           "deny"
   "Content-Encoding"          "zstd"})

(defn wrap-response-headers [handler csp-overrides]
  (let [default-headers
        (assoc default-headers
          "Content-Security-Policy"
          (csp-data->str (u/fast-merge csp-data csp-overrides)))
        merge-defaults     (fn [x] (u/fast-merge default-headers x))
        empty-body-headers {"Strict-Transport-Security" strict-transport
                            "Cache-Control"             "no-store"}]
    (fn [req]
      (let [resp (handler req)]
        (if (:body resp)
          (update resp :headers merge-defaults)
          ;; If we don't have a body we still want to be strict and no-store
          (assoc resp :headers empty-body-headers))))))
