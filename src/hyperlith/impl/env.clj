(ns hyperlith.impl.env
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def env-data
  (when-let  [env-file (io/resource ".env.edn")]
    (-> env-file slurp edn/read-string)))

(defmacro env
  "Read env from .env.edn. If env is missing fails at compile time."
  [k]
  (if (k env-data)
    ;; We could just inline the value, but that makes it trickier
    ;; to patch env values on a running server from the REPL.
    `(env-data ~k)
    (throw (ex-info (str "Missing env in .env.edn: " k)
             {:missing-env k}))))

