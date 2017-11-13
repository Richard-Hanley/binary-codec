(ns binary-codec.core
  (:import (java.nio ByteBuffer
                     ByteOrder))
  (:require [clojure.spec.alpha :as s]))

(def byteorders {:endian/little ByteOrder/LITTLE_ENDIAN
                 :endian/big ByteOrder/BIG_ENDIAN
                 :endian/native (ByteOrder/nativeOrder)
                 :endian/network ByteOrder/BIG_ENDIAN})

(defrecord Encoder [data op size alignment])

(defprotocol Codec
  (to-encoder* [this data alignment] "Validates and processes a given piece of data, returning an Encoder")
  (decoder* [this] "I don't know what this function will look like"))


(defonce ^:private registry-ref (atom {}))

(defn registry
    "returns the registry map, prefer 'get-codec' to lookup a codec by name"
    []
    @registry-ref)

(defn- with-name [spec name]
    (with-meta spec (assoc (meta spec) ::name name)))

(defn- named? [x] (instance? clojure.lang.Named x))

(defn- reg-resolve
    "returns the codec end of alias chain starting with k, nil if not found, k if k not Named"
    [k]
    (if (named? k)
      (let [reg @registry-ref]
        (loop [spec k]
          (if (named? spec)
            (recur (get reg spec))
            (when spec
              (with-name spec k)))))
      k))

(defmacro def 
  "Macro used to add codecs to a spec style registry.  If a specification
  is supplied, then it will be defined in spec as well.  If no spec is passed
  and there is nothing in the spec registry, then identity will be used"
  ([k codec] 
   `(do
      (swap! registry-ref assoc ~k ~codec)
      (if (not (s/get-spec k))
        (s/def ~k identity))
      ~k))
  ([k codec spec]
    `(do
      (swap! registry-ref assoc ~k ~codec)
      (if (not= ~k ~spec)
        (s/def ~k ~spec))
      ~k)))

(s/def ::foo even?)

(binary-codec.core/def ::int8 
  (reify Codec
    (to-encoder* [_ data alignment] )
    (decoder* [_] ))
   (s/conformer #(try (byte %) (catch IllegalArgumentException e ::s/invalid))))
 
; (def int16
;   (reify Codec
;     (alignment [_ encoding] (Byte/BYTES))
;     (to-bytes [_ encoding data] (byte-array [data]))
;     (from-bytes [_ encoding binary] [(first binary) (rest binary)])))

(defn to-encoder [codec-or-k]
  (let [codec (reg-resolve codec-or-k)]



(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
