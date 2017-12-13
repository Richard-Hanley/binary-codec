(ns binary-codec.core
  (:import (java.nio ByteBuffer
                     ByteOrder))
  (:require [binary-codec.encoding :as encoding]
            [clojure.spec.alpha :as s]))

(defprotocol Codec
  (encode*        [this encoding]     "Encodes a codec with the given parameters, and returns a new codec")

  (encoded?*      [this]              "Returns a boolean of whether or not the codec is fully encoded, or if it
                                      needs more encoding parameters")

  (alignment*     [this]              "The alignment of this codec")

  (sizeof*        [this] [this data]  "What is the number of bytes that this codec will fill.
                                      If the size is variable, then the data will need to be passed
                                      in, otherwise the result will be nil")

  (to-buffer!*    [this data buffer]  "Serialize data to a passed in buffer")

  (from-buffer!*  [this buffer]       "Deserialize from a binary buffer to data"))

(defn alignment-padding [align-to position]
  (if-not (zero? align-to)
    (let [offset (mod position align-to)]
      (if-not (zero? offset)
        (- align-to offset)
        0))
    0))

(defn- align-buffer-write [align-to buffer]
  (let [position (.position buffer)
        padding (alignment-padding align-to position)]
    (dotimes [n padding]
      (.put buffer (byte 0)))))

(defn- align-buffer-read [align-to buffer]
  (let [position (.position buffer)
        padding (alignment-padding align-to position)]
    (dotimes [n padding] 
      (.get buffer))))

(defn- make-signed-integral-conformer [checked-converter]
  (s/conformer (fn [value]
                 (try 
                   (checked-converter value)
                   (catch Exception e ::s/invalid)))))

(defn- make-unsigned-integral-conformer [bit-length checked-converter unchecked-converter]
  (let [shift-value (- Long/SIZE bit-length)
        sized-bitmask (unsigned-bit-shift-right -1 shift-value)]
    (s/conformer 
      (fn [value]

        (try 
          (checked-converter value)
          (catch IllegalArgumentException e 
            (if (= (bit-and value sized-bitmask) value)
              (unchecked-converter value)
              ::s/invalid))
          (catch Exception e ::s/invalid))))))

(defn codec?
  "returns c if c is a codec, else false"
  [c]
  (and (satisfies? Codec c) c))

(defn codec-spec 
  "Given a codec codec-spec will return an associated spec, or nil.
  If the given codec is a keyword, then this will return the same keyword
  if and only if the keyword is registered with spec

  Otherwise codec-spec will look at the :spec field in the objects metadata"
  [c]
  (if (and (keyword? c) (s/get-spec c))
    c
    (:spec (meta c))))

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

(defn defcodec [k c]
  (swap! registry-ref assoc k c))

(defmacro def 
  "Function used to define a global codec, a la spec.  It takes a fully qualified keyword
  as and a codec, and adds that codec to the registry

  If the codec also has a spec associated with it, then that spec will also be registered with
  the same keyword
  
  If a spec is also given as an argument, then it will be combined with the codec spec"
  ([k c] 
   (let [spec `(codec-spec ~c)]
     `(do
        (defcodec ~k ~c)
        (if (some? ~spec)
          (s/def ~k ~spec))
        ~k)))
  ([k c s]
   (let [cs `(codec-spec ~c)
         spec `(if (some? ~cs)
                (s/and ~s ~cs)
                ~s)]
     `(do
        (defcodec ~k ~c)
        (if (some? ~spec)
          (s/def ~k ~spec))
        ~k))))

(defn seq-to-field-map [field-keys]
  (into (array-map) (map vector field-keys field-keys)))

(defmacro defstruct [k & fields]
  (let [field-keys (map eval fields)
        field-map (seq-to-field-map field-keys)]
    `(do
       (s/def ~k (s/keys :req [~@field-keys]))
       (defcodec ~k ~field-map))))

(defn encode [codec-or-k encoding]
   (if-let [codec (reg-resolve codec-or-k)]
     (encode* codec encoding)
     (throw (Exception. (str "Unable to resolve codec - " codec-or-k)))))

(defn encoded? [codec-or-k]
   (if-let [codec (reg-resolve codec-or-k)]
     (encoded?* codec)
     (throw (Exception. (str "Unable to resolve codec - " codec-or-k)))))

(defn alignment
  ([codec-or-k] 
   (if-let [codec (reg-resolve codec-or-k)]
     (alignment* codec)
     (throw (Exception. (str "Unable to resolve codec - " codec-or-k))))))

(defn sizeof 
  ([codec-or-k] 
   (if-let [codec (reg-resolve codec-or-k)]
     (sizeof* codec)
     (throw (Exception. (str "Unable to resolve codec - " codec-or-k)))))
  ([codec-or-k data]
   (if-let [codec (reg-resolve codec-or-k)]
     (sizeof* codec data)
     (throw (Exception. (str "Unable to resolve codec - " codec-or-k))))))

(defn to-buffer!
  ([codec-or-k data buffer] 
   (if-let [codec (reg-resolve codec-or-k)]
     (do
       (align-buffer-write (alignment* codec) buffer)
       (to-buffer!* codec data buffer))
     (throw (Exception. (str "Unable to resolve codec - " codec-or-k))))))

(defn from-buffer!
  ([codec-or-k buffer] 
   (if-let [codec (reg-resolve codec-or-k)]
     (do
       (align-buffer-read (alignment* codec) buffer)
       (from-buffer!* codec buffer))
     (throw (Exception. (str "Unable to resolve codec - " codec-or-k))))))

(defn encode-primitive [codec primitive-encoding]
  (let [conformed-encoding (if (s/valid? ::encoding/base-encoding primitive-encoding)
                             (s/conform ::encoding/base-encoding primitive-encoding)
                             (throw (Exception. (str "Unable to encode primitive" 
                                                   (s/explain-str ::encoding/base-encoding primitive-encoding)))))
        primitive-alignment (if-let [word-size (::encoding/word-size conformed-encoding)]
                              (min word-size (sizeof codec))
                              0)]
    (reify Codec
      (encode* [_ encoding] (encode codec (merge primitive-encoding encoding)))
      (encoded?* [_] (encoded? codec))
      (alignment* [_] 
        ;Use the primitive alignment, unless this codec was already aligned to a larger value
        (max (alignment codec) primitive-alignment))
      (sizeof* [_] (sizeof codec))
      (sizeof* [_ data] (sizeof codec data))
      (to-buffer!* [_ data buffer] 
        (if-let [byte-order (::encoding/byte-order conformed-encoding)]
          (encoding/buffer-op-with-endian byte-order
                                          (partial to-buffer! codec data)
                                          buffer)
          (to-buffer! codec data buffer)))
      (from-buffer!* [_ buffer] 
        (if-let [byte-order (::encoding/byte-order conformed-encoding)]
          (encoding/buffer-op-with-endian byte-order
                                          (partial from-buffer! codec)
                                          buffer)
          (from-buffer! codec buffer))))))

(binary-codec.core/def
  ::int8 
  (reify Codec
    (encode* [this encoding] (encode-primitive this encoding))
    (encoded?* [_] true)
    (alignment* [_] 0)
    (sizeof* [_] Byte/BYTES)
    (sizeof* [_ _] Byte/BYTES)
    (to-buffer!* [_ data buffer] (.put buffer data))
    (from-buffer!* [_ buffer] (.get buffer)))
  (make-signed-integral-conformer byte))

(binary-codec.core/def
  ::int16
  (reify Codec
    (encode* [this encoding] (encode-primitive this encoding))
    (encoded?* [_] true)
    (alignment* [_] 0)
    (sizeof* [_] Short/BYTES)
    (sizeof* [_ _] Short/BYTES)
    (to-buffer!* [_ data buffer] (.putShort buffer data))
    (from-buffer!* [_ buffer] (.getShort buffer)))
  (make-signed-integral-conformer short))

(binary-codec.core/def 
  ::int32
  (reify Codec
    (encode* [this encoding] (encode-primitive this encoding))
    (encoded?* [_] true)
    (alignment* [_] 0)
    (sizeof* [_] Integer/BYTES)
    (sizeof* [_ _] Integer/BYTES)
    (to-buffer!* [_ data buffer] (.putInt buffer data))
    (from-buffer!* [_ buffer] (.getInt buffer)))
  (make-signed-integral-conformer int))

(binary-codec.core/def 
  ::int64
  (reify Codec
    (encode* [this encoding] (encode-primitive this encoding))
    (encoded?* [_] true)
    (alignment* [_] 0)
    (sizeof* [_] Long/BYTES)
    (sizeof* [_ _] Long/BYTES)
    (to-buffer!* [_ data buffer] (.putLong buffer data))
    (from-buffer!* [_ buffer] (.getLong buffer)))
  (make-signed-integral-conformer long))

(binary-codec.core/def ::uint8 ::int8 (make-unsigned-integral-conformer Byte/SIZE byte unchecked-byte))
(binary-codec.core/def ::uint16 ::int16 (make-unsigned-integral-conformer Short/SIZE short unchecked-short))
(binary-codec.core/def ::uint32 ::int32 (make-unsigned-integral-conformer Integer/SIZE int unchecked-int))

(defn lazy-pad
  "Returns a lazy sequence which pads sequence with pad-value."
  [sequence pad-value]
  (if (empty? sequence)
    (repeat pad-value)
    (lazy-seq (cons (first sequence) (lazy-pad (rest sequence) pad-value)))))

(extend-type clojure.lang.Sequential
  Codec
  (encode* [this encoding] (map #(encode % encoding) this))
  (encoded?* [this] (every? encoded? this))
  (alignment* [this] (apply max (map alignment this)))
  (sizeof* 
    ([this]
     (reduce 
       (fn [accum codec]
         (if-let [size (sizeof codec)]
           (+ accum size (alignment-padding (alignment codec) accum))
           (reduced nil)))
       0
       this))
    ([this data]
     (reduce 
       (fn [accum [codec elem]]
         (if-let [size (sizeof codec elem)]
           (+ accum size (alignment-padding (alignment codec) accum))
           (reduced nil)))
       0
       (map vector this (lazy-pad data nil)))))
  (to-buffer!* [this data buffer] 
    (doseq [[codec elem] (map vector this data)]
      (to-buffer! codec elem buffer))
    buffer)
  (from-buffer!* [this buffer]
    (into [] (doall (map #(from-buffer! % buffer) this)))))

(extend-type clojure.lang.PersistentArrayMap
  Codec
  (encode* [this encoding] (zipmap (keys this) (map #(encode % encoding) (vals this))))
  (encoded?* [this] (every? encoded? (vals this)))
  (alignment* [this] 
    (alignment (vals this)))
  (sizeof* 
    ([this] (sizeof (vals this)))
    ([this data] (sizeof (vals this) (vals data))))
  (to-buffer!* [this data buffer] 
    (to-buffer! (vals this) (map #(get data %1) (keys this)) buffer))
  (from-buffer!* [this buffer] 
    (let [values (from-buffer! (vals this) buffer)]
      (into {} (map vector (keys this) values)))))

(defn tagged-union [base-codec dispatch]
  (reify Codec
    (encode* [this encoding] 
      ;Return codec with this's current spec as metadata
      (with-meta 
        this
        (assoc (meta encoding) :spec (codec-spec this))))
    (encoded?* [this] true)
    (alignment* [_] (alignment base-codec))
    (sizeof* [_] nil)
    (sizeof* [_ data] 
      (if (nil? data)
        nil
        (sizeof (dispatch data) data)))
    (to-buffer!* [_ data buffer] (to-buffer! (dispatch data) data buffer))
    (from-buffer!* [_ buffer]
      (let [base-data (from-buffer! base-codec buffer)
            full-codec (dispatch base-data)
            base-read-length (sizeof base-codec base-data)
            buffer-position (.position buffer)
            unwound-buffer (.position buffer (- buffer-position base-read-length))]
        (from-buffer! full-codec buffer)))))

(defn align [align-to codec]
  (reify Codec
    (encode* [this encoding] 
      ;Return codec with this's current spec as metadata
      (with-meta 
        (align align-to (encode codec encoding))
        (assoc (meta encoding) :spec (codec-spec this))))
    (encoded?* [this] (encoded? codec))
    (alignment* [_] 
      (let [old-alignment (alignment codec)
            new-alignment (max align-to old-alignment)]
        (if (= 0 (mod new-alignment old-alignment))
          new-alignment
          (throw (IllegalArgumentException.
                   (format "cannot align to new alignment (%d) because it is not an even multiple of the old alignment (%d)" new-alignment old-alignment))))))
                                               

    (sizeof* [_] (sizeof codec))
    (sizeof* [_ data] (sizeof codec data))
    (to-buffer!* [_ data buffer] (to-buffer! codec data buffer))
    (from-buffer!* [_ buffer] (from-buffer! codec buffer))))

(defn unaligned [codec]
  (reify Codec
    (encode* [this encoding] 
      ;Return codec with this's current spec as metadata
      (with-meta 
        (unaligned (encode codec encoding)) 
        (assoc (meta encoding) :spec (codec-spec this))))
    (encoded?* [this] (encoded? codec))
    (alignment* [_] 0)
    (sizeof* [_] (sizeof codec))
    (sizeof* [_ data] (sizeof codec data))
    (to-buffer!* [_ data buffer] (to-buffer! codec data buffer))
    (from-buffer!* [_ buffer] (from-buffer! codec buffer))))


