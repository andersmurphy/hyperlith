(ns hyperlith.impl.session
  (:require [hyperlith.impl.crypto :as crypto]))

(defn get-sid [req]
  (try ;; In case we get garbage
    (some->> ((:headers req) "cookie")
      (re-find #"__Host-sid=([^;^ ]+)")
      second)
    (catch Throwable _)))

(defn session-cookie [sid]
  (str "__Host-sid=" sid "; Path=/; Secure; HttpOnly; SameSite=Lax"))

(defn wrap-session
  [handler]
  (fn [req]
    (let [body (:body req)
          sid  (get-sid req)]
      (cond
        ;; If user has sid and csrf handle request
        sid
        (handler (assoc req :sid sid :csrf (:csrf body) :tabid (:tabid body)))

        ;; :get request and user does not have session we create one
        ;; if they do not have a csrf cookie we give them one
        (= (:request-method req) :get)
        (let [new-sid (or sid (crypto/random-unguessable-uid))]
          (-> (handler (assoc req :sid new-sid :tabid (:tabid body)))
            (assoc-in [:headers "Set-Cookie"]
              ;; These cookies won't be set on local host on chrome/safari
              ;; as it's using secure needs to be true and local host
              ;; does not have HTTPS. SameSite is set to lax as it
              ;; allows the same cookie session to be used following a
              ;; link from another site.
              [(session-cookie new-sid)])))

        ;; Not a :get request and user does not have session we 403
        ;; Note: If the updates SSE connection is a not a :get then this
        ;; will close the connection until the user reloads the page.
        :else
        {:status 403}))))
