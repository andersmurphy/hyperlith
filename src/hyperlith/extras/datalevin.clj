(ns hyperlith.extras.datalevin
  "Datalevin wrapper for hyperlith. Requires datalevin 9.15+ as a dependency."
  (:require [hyperlith.core :as h]
            [datalevin.core :as d]
            [datalevin.lmdb :as l])
  (:import [datalevin.db DB]
           [datalevin.storage Store]))

(def default-schema
  (merge
    #:session
    {:id   {:db/unique      :db.unique/identity
            :db/valueType   :db.type/string
            :db/cardinality :db.cardinality/one}
     :user {:db/valueType   :db.type/ref
            :db/cardinality :db.cardinality/one}}

    #:user
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

(defn ctx-start
  ([name schema]
   (ctx-start name schema {} {}))
  ([name schema ctx]
   (ctx-start name schema ctx {}))
  ([name schema opts ctx]
   (let [db (d/get-conn name schema
              (merge
                {:validate-data?    true
                 :closed-schema?    true
                 :auto-entity-time? true} opts))]
     (d/listen! db :refresh-on-change
       (fn [_] (h/refresh-all!)))
     (merge ctx {:db db}))))

(defn ctx-stop [{:keys [db] :as _ctx}]
  (d/close db))

(defn log-on-error [{:keys [db] :as _ctx} {:keys [req error]}]
  (let [sid (or (:sid req) "no-sid")
        txs [{:session/id sid} ;; users might not have a session in the db
             (h/qualify-keys
               (dissoc error :via)
               :error)
             (h/qualify-keys
               {:session [:session/id sid]
                :error   [:error/id (:id error)]}
               :error-event)]]
    (try ;; tx! can fail if data contains un-serialisable items
      @(tx! db txs)
      (catch Throwable _
        ;; if data elements can't be serialized we remove them
        @(tx! db (update txs 1 dissoc :error/data))))))

(defn backup-copy!
  "Make a backup copy of the database. `dest-dir` is the destination
  data directory path. Will compact while copying if `compact?` is true."
  [conn dest-dir compact?]
  (let [lmdb (.-lmdb ^Store (.-store ^DB conn))]
    (println "Copying...")
    (l/copy lmdb dest-dir compact?)
    (println "Copied database.")))

(defn tuples
  "Returns the set of tuples that represent a nested map/vector. Lets you
  use datalog query engines to query json/edn data."
  ([root] {:pre [(or (map? root) (vector? root))]}
   (tuples [] root))
  ([parent x]
   (cond (map? x)
         (mapcat (fn [[k v]] (tuples (conj parent k) v)) x)

         (vector? x)
         (mapcat (fn [i v] (tuples (conj parent i) v)) (range) x)

         :else [(conj parent x)])))


