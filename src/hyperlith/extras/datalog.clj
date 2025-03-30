(ns hyperlith.extras.datalog
  "Datalevin (datalog db) wrapper for hyperlith. Requires datalevin 9.15+ as
  a dependency."
  (:require [hyperlith.core :as h]
            [datalevin.core :as d]
            [datalevin.lmdb :as l])
  (:import [datalevin.db DB]
           [datalevin.storage Store]))

(def default-schema
  (merge
    #:session
    {:id {:db/unique      :db.unique/identity
          :db/valueType   :db.type/string
          :db/cardinality :db.cardinality/one}}

    #:error
    {:id    {:db/unique      :db.unique/identity
             :db/valueType   :db.type/string
             :db/cardinality :db.cardinality/one}
     :cause {:db/valueType   :db.type/string
             :db/cardinality :db.cardinality/one}
     :trace {;; No type so that trace can be stored as edn
             :db/cardinality :db.cardinality/one}
     :type  {:db/valueType   :db.type/string
             :db/cardinality :db.cardinality/one}
     :data  {;; No type so that trace can be stored as edn
             :db/cardinality :db.cardinality/one}}

    #:error-event
    {;; Reified join so we get the date of when it happened
     :session {:db/valueType   :db.type/ref
               :db/cardinality :db.cardinality/one}
     :error   {:db/valueType   :db.type/ref
               :db/cardinality :db.cardinality/one}}))

(def q d/q)
(def tx! d/transact-async)

(defn ctx-start [name schema & [ctx]]
  (let [db (d/get-conn name schema
             {:validate-data?    true
              :closed-schema?    true
              :auto-entity-time? true})]
    (d/listen! db :refresh-on-change
      (fn [_] (h/refresh-all!)))
    (merge ctx {:db db})))

(defn ctx-stop [{:keys [db] :as _ctx}]
  (d/close db))

(defn log-on-error [{:keys [db] :as _ctx} {:keys [req error]}]
  (let [sid (or (:sid req) "no-sid")
        txs [{:session/id sid} ;; users might not have a session in the db
             (h/qualify-keys
               (dissoc error :data :via) ;; some data elements don't serialize
               :error)
             (h/qualify-keys
               {:session [:session/id sid]
                :error   [:error/id (:id error)]}
               :error-event)]]
    @(tx! db txs)))

(defn backup-copy!
  "Make a backup copy of the database. `dest-dir` is the destination
  data directory path. Will compact while copying if `compact?` is true."
  [conn dest-dir compact?]
  (let [lmdb (.-lmdb ^Store (.-store ^DB conn))]
    (println "Copying...")
    (l/copy lmdb dest-dir compact?)
    (println "Copied database.")))




