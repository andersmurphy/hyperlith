(ns hyperlith.impl.headers
  (:require [clojure.string :as str]))

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
   :style-src       [self]
   ;; We only use this because I'm currently using intline style-src-elem
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
  {"Content-Type"              "text/html"
   "Strict-Transport-Security" strict-transport
   "Content-Security-Policy"   (csp-data->str csp-data)
   "Referrer-Policy"           "no-referrer"
   "X-Content-Type-Options"    "nosniff"
   "X-Frame-Options"           "deny"
   "Cache-Control"             "no-cache, must-revalidate"})
