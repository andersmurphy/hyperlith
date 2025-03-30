(ns app.schema
  (:require [hyperlith.extras.datalog :as d]))

(def schema
  (merge ;; Using merge we can define each logical entity separately
    d/default-schema
    #:message
    {:id      {:db/unique      :db.unique/identity
               :db/valueType   :db.type/string
               :db/cardinality :db.cardinality/one}
     :user    {:db/valueType   :db.type/ref
               :db/cardinality :db.cardinality/one}
     :content {:db/valueType   :db.type/string
               :db/cardinality :db.cardinality/one
               :db/fulltext    true}}))
