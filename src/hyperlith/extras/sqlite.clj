(ns hyperlith.extras.sqlite
  (:require [hyperlith.impl.namespaces :refer [import-vars]]
            [sqlite4clj.core :as d]
            [honey.sql :as hsql]
            [clojure.java.process :as proc]
            [clojure.java.io :as io])
  (:import [java.nio.file Files]))

(import-vars
  [sqlite4clj.core
   init-db!
   with-read-tx
   with-write-tx])

(defmacro q [db [query-type query :as string-query] & [params]]
  (if (string? query-type)
    `(d/q ~db ~string-query)
    `(d/q ~db ~(hsql/format query {:params params ;; :checking :strict
                                   }))))

(def format-query hsql/format)

;;; - UTILITY -

(defn table-info [db table-name]
  (let [t-name (-> table-name name)]
    (q db ["PRAGMA table_info(?);" t-name])))

(defn table-list [db]
  (q db ["PRAGMA table_list;"]))

(defn pragma-check [db]
  (->> [(q db ["pragma foreign_keys"])
        (q db ["pragma journal_mode"])
        (q db ["pragma synchronous"])
        (q db ["pragma page_size"])
        (q db ["pragma cache_size"])
        (q db ["pragma temp_store"])]))

(defn copy-resource [resource-path output-path]
  (with-open [in  (io/input-stream (io/resource resource-path))
              out (io/output-stream (io/file output-path))]
    (io/copy in out)))

(def litestream-yml-template
  "dbs:
    - path: %s
      replicas:
       - type: s3
         bucket:   %s
         path:     %s
         endpoint: %s
         region:   %s
         sync-interval: 1s")

(defn init-litestream!
  "This is a noop if db already exists. Throws error if litestream is not
  present. You want to know if your backups are not working."
  [db-name {:keys [s3-access-key-id s3-access-secret-key
                   bucket endpoint region]}]
  (let [config-file "litestream_temp.yml"
        _           (spit config-file (format litestream-yml-template
                                        db-name bucket db-name endpoint region))
        process     (proc/start
                      {:env {"AWS_ACCESS_KEY_ID"     s3-access-key-id
                             "AWS_SECRET_ACCESS_KEY" s3-access-secret-key}}
                      "litestream" "replicate" "-config" config-file)]
    (proc/exec {:env {"AWS_ACCESS_KEY_ID"     s3-access-key-id
                      "AWS_SECRET_ACCESS_KEY" s3-access-secret-key}}
      "litestream" "restore" "-if-db-not-exists"
      "-if-replica-exists" "-config" config-file db-name)
    ;; We delete config once loaded
    (Files/deleteIfExists (.toPath (io/file config-file)))
    ;; Return litestream process
    process))


(comment
  ;; Example of how to print process data

  #_{:clj-kondo/ignore [:unresolved-symbol]}
  (with-open
    [rdr (-> (proc/start {:env {"AWS_ACCESS_KEY_ID"     s3-access-key-id
                                "AWS_SECRET_ACCESS_KEY" s3-access-secret-key}}
               "litestream" "restore" "-config" config-file db-name)
             proc/stdout
             io/reader)]
    (run! (fn [x] (println x)) (line-seq rdr))))
