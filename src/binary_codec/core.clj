(ns binary-codec.core
  (:import (java.nio ByteBuffer
                     ByteOrder))
  (:require [clojure.spec.alpha :as s]))

(defprotocol Codec
  (alignment*     [this encoding] "The alignment of this codec with a given encoding")
  (sizeof*        [this encoding data] "Given a piece of data, and an encoding, 
                                       what is the number of bytes that it will fill")
  (to-buffer!*    [this encoding data buffer] "Serialize data to a passed in buffer, based on the
                                              associated encoding")
  (from-buffer!*  [this encoding buffer] "Deserialize from a binary buffer to data, based on the
                                         associated encoding"))

(defn- align-buffer-write [align-to buffer]
  (if-not (zero? align-to)
    (let [position (.position buffer)
          padding (- align-to (mod position align-to))]
      (dotimes [n padding] 
        (.put buffer (byte 0))))))

(defn- align-buffer-read [align-to buffer]
  (if-not (zero? align-to)
    (let [position (.position buffer)
          padding (mod position align-to)]
      (dotimes [n padding] 
        (println "reading a byte of padding")
        (.get buffer (byte 0))))))

(defn codec?
  "returns c if c is a codec, else false"
  [c]
  (and (extends? Codec (class c)) c))
 

(defonce ^:private registry-ref (atom {}))

(defn registry
    "returns the registry map, prefer 'get-codec' to lookup a codec by name"
    []
    @registry-ref)

(defn- with-name [spec name]
    (with-meta spec (assoc (meta spec) ::name name)))

(defn- named? [x] (instance? clojure.lang.Named x))

(defn reg-resolve
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
  "Macro used to define a global codec, a la spec.  It takes a fully qualified keyword
  as and a codec, and adds that codec to the registry"
  [k codec] 
   `(do
      (swap! registry-ref assoc ~k ~codec)
      ~k))

(s/def ::word-size #{0 1 2 4 8})

(s/def ::base-encoding (s/keys :req [::word-size]))

(def base-encoding {::word-size 0})

(defn alignment
  ([codec-or-k] (alignment codec-or-k base-encoding))
  ([codec-or-k encoding] 
   (if-let [codec (reg-resolve codec-or-k)]
     (alignment* codec encoding)
     (throw (Exception. (str "Unable to resolve codec - " codec-or-k))))))

(defn sizeof 
  ([codec-or-k] (sizeof codec-or-k base-encoding nil))
  ([codec-or-k encoding] (sizeof codec-or-k encoding nil))
  ([codec-or-k encoding data]
   (if-let [codec (reg-resolve codec-or-k)]
     (sizeof* codec encoding data)
     (throw (Exception. (str "Unable to resolve codec - " codec-or-k))))))

(defn to-buffer!
  ([codec-or-k data buffer] (to-buffer! codec-or-k base-encoding data buffer))
  ([codec-or-k encoding data buffer]
   (if-let [codec (reg-resolve codec-or-k)]
     (do
       (align-buffer-write (alignment* codec encoding) buffer)
       (to-buffer!* codec encoding data buffer))
     (throw (Exception. (str "Unable to resolve codec - " codec-or-k))))))

(defn from-buffer!
  ([codec-or-k buffer] (from-buffer! codec-or-k base-encoding buffer))
  ([codec-or-k encoding buffer]
   (if-let [codec (reg-resolve codec-or-k)]
     (do
       (align-buffer-read (alignment* codec encoding) buffer)
       (from-buffer!* codec encoding buffer))
     (throw (Exception. (str "Unable to resolve codec - " codec-or-k))))))

(binary-codec.core/def ::int8 
  (reify Codec
    (alignment* [_ _] 0)
    (sizeof* [_ _ _] Byte/BYTES)
    (to-buffer!* [_ _ data buffer] (.put buffer data))
    (from-buffer!* [_ _ buffer] (.get buffer))))
 
(binary-codec.core/def ::int16
  (reify Codec
    (alignment* [_ encoding] 
      (let [{word-size ::word-size} (s/conform ::base-encoding encoding)]
        (min word-size Short/BYTES)))
    (sizeof* [_ _ _] Short/BYTES)
    (to-buffer!* [_ _ data buffer] (.putShort buffer data))
    (from-buffer!* [_ _ buffer] (.getShort buffer))))
 
(binary-codec.core/def ::int32
  (reify Codec
    (alignment* [_ encoding]
      (let [{word-size ::word-size} (s/conform ::base-encoding encoding)]
        (min word-size Integer/BYTES)))
    (sizeof* [_ _ _] Integer/BYTES)
    (to-buffer!* [_ _ data buffer] (.putInt buffer data))
    (from-buffer!* [_ _ buffer] (.getInt buffer))))

(binary-codec.core/def ::int64
  (reify Codec
    (alignment* [_ encoding]
      (let [{word-size ::word-size} (s/conform ::base-encoding encoding)]
        (min word-size Long/BYTES)))
    (sizeof* [_ _ _] Long/BYTES)
    (to-buffer!* [_ _ data buffer] (.putLong buffer data))
    (from-buffer!* [_ _ buffer] (.getLong buffer))))
