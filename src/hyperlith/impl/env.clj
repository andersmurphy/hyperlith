(ns hyperlith.impl.env
  (:require [clojure.edn :as edn]
            [hyperlith.impl.util :as u]))

(def env-data
  (-> (u/load-resource ".env.edn") slurp edn/read-string))

(defmacro env
  "Read env from .env.edn. If env is missing fails at compile time."
  [k]
  (if (env-data k)
    ;; We could just inline the value, but that makes it trickier
    ;; to patch env values on a running server from the REPL.
    `(env-data ~k)
    (throw (ex-info (str "Missing env in .env.edn: " k)
             {:missing-env k}))))

