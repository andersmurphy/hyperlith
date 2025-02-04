(ns hyperlith.impl.crypto
  (:import [javax.crypto Mac]
           [javax.crypto.spec SecretKeySpec]
           [java.security SecureRandom]
           [java.util Base64 Base64$Encoder]))

(def ^SecureRandom secure-random
  (SecureRandom/new))

(def ^Base64$Encoder base64-encoder
  (.withoutPadding (Base64/getUrlEncoder)))

(defn bytes->base64 [^byte/1 b]
  (.encodeToString base64-encoder b))

(defn random-unguessable-uid
  "URL-safe base64-encoded 160-bit (20 byte) random value. Speed
  is similar random-uuid.
  See: https://neilmadden.blog/2018/08/30/moving-away-from-uuids/"
  []
  (let [buffer (byte-array 20)]
    (.nextBytes secure-random buffer)
    (bytes->base64 buffer)))

(defn secret-key->hmac-md5-keyspec [secret-key]
  (SecretKeySpec/new (String/.getBytes secret-key) "HmacMD5"))

(defn hmac-md5
  "Used for quick stateless csrf token generation."
  [key-spec data]
  (-> (doto (Mac/getInstance "HmacMD5")
        (.init key-spec))
    (.doFinal (String/.getBytes data))
    bytes->base64))

(defn digest
  "Digest function based on Clojure's hash."
  [data]
  (bytes->base64 (.getBytes (str (hash data)))))
