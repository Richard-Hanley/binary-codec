(ns binary-codec.core
  (:import (java.nio ByteBuffer
                     ByteOrder))
  (:require [clojure.spec.alpha :as s]))

(defprotocol Codec
  (encoder* [this encoding])
  (alignment* [this encoding])
  (sizeof* [this encoding] [this encoding data])
  (to-buffer!* [this encoding data buffer])
  (from-buffer!* [this encoding buffer]))

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

  Otherwise codec-spec will look at the :spec field in the objects metadata
  
  If there is no spec associated with the codec, then it will use identity"
  [c]
  (if (and (keyword? c) (s/get-spec c))
    c
    (or (:spec (meta c) identity))))

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

(defn get-codec [k]
  (get @registry-ref k))

(defn specified-codec?
  "Returns the argument if the arg is either a reified codec with a spec in it's metadata
  or if it is a keyword that is registered with both binary-codec.core and spec"
  [k-or-codec]
  (let [registered-keyword (and (keyword? k-or-codec)
                                (s/get-spec k-or-codec)
                                (get-codec k-or-codec)) 
        codec (some? (:spec (meta (codec? k-or-codec))))]
    (if (or registered-keyword codec)
      k-or-codec
      nil)))

(defn defcodec [k c]
  (swap! registry-ref assoc k c))


(defn spec-maybe-auto [spec]
  "Takes a spec, and wraps it so that the result may be auto"
  (s/conformer
    (fn [value]
      (if (= value ::auto)
        value
        (s/conform spec value)))))


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
        (s/def ~k (spec-maybe-auto ~spec))
        ~k)))
  ([k c s]
   (let [cs `(codec-spec ~c)
         spec `(s/and ~s ~cs)]
     `(do
        (defcodec ~k ~c)
        (s/def ~k (spec-maybe-auto ~spec))
        ~k))))

(defn- resolve-codec [codec-or-k]
  (if-let [codec (reg-resolve codec-or-k)]
    codec
    (throw (Exception. (str "Unable to resolve codec - " codec-or-k)))))

(defn encoder
  "Returns the encoding used by the passed reified codec or registered keyword.
  If no encoding is passed, then the default encoding is used.  Otherwise the passed
  encoding is merged with the default codec, and then validated."
  ([codec-or-k] (encoder codec-or-k nil))
  ([codec-or-k encoding] (encoder* (resolve-codec codec-or-k) encoding)))

(defn alignment
  "Gets the alignment of a codec with a given encoding.  If no encoding is specified
  then the default encoding is used"
  ([codec-or-k] (alignment codec-or-k nil))
  ([codec-or-k encoding] 
   (let [codec (resolve-codec codec-or-k)]
     (alignment* codec (encoder* codec encoding)))))

(defn unchecked-alignment
  "Same as alignment, but it requires the encoding to be specified, and does
  not do any validation of the encoding"
  [codec-or-k encoding]
   (let [codec (resolve-codec codec-or-k)]
     (alignment* codec encoding)))

(defn sizeof 
  "Gets the size of the codec with a given encoding. If no encoding is specified then
  the default encoding is used.  Some codecs do no have static sizes, and so they must
  have the target data passed in as well"
  ([codec-or-k] (sizeof codec-or-k nil))
  ([codec-or-k encoding] 
   (let [codec (resolve-codec codec-or-k)]
     (sizeof* codec (encoder* codec encoding))))
  ([codec-or-k encoding data] 
   (let [codec (resolve-codec codec-or-k)]
     (sizeof* codec (encoder* codec encoding) data))))

(defn unchecked-sizeof
  "Same as sizeof, but it requires the encoding to be specified, and does
  not do any validation of the encoding"
  ([codec-or-k encoding] 
   (let [codec (resolve-codec codec-or-k)]
     (sizeof* codec encoding)))
  ([codec-or-k encoding data] 
   (let [codec (resolve-codec codec-or-k)]
     (sizeof* codec encoding data))))

(defn to-buffer!
  "Writes data to a java.nio.ByteBuffer using the given codec and encoding.
  If no encoding is specified then the default encoding is used"
  ([codec-or-k data buffer] (to-buffer! codec-or-k nil data buffer))
  ([codec-or-k encoding data buffer]
   (let [codec (resolve-codec codec-or-k)
         enc (encoder* codec encoding)]
     (do
       (align-buffer-write (alignment* codec enc) buffer)
       (to-buffer!* codec enc data buffer)))))

(defn unchecked-to-buffer!
  "Same as to-buffer!, but it requires the encoding to be specified, and does
  not do any validation of the encoding"
  [codec-or-k encoding data buffer]
   (let [codec (resolve-codec codec-or-k)]
     (do
       (align-buffer-write (alignment* codec encoding) buffer)
       (to-buffer!* codec encoding data buffer))))

(defn from-buffer!
  "Reads data from a java.nio.ByteBuffer using the given codec and encoding.
  If no encoding is specified then the default encoding is used"
  ([codec-or-k buffer] (from-buffer! codec-or-k nil buffer))
  ([codec-or-k encoding buffer]
   (let [codec (resolve-codec codec-or-k)
         enc (encoder* codec encoding)]
     (do
       (align-buffer-read (alignment* codec enc) buffer)
       (from-buffer!* codec enc buffer)))))

(defn unchecked-from-buffer!
  "Same as from-buffer!, but it requires the encoding to be specified, and does
  not do any validation of the encoding"
  [codec-or-k encoding buffer]
   (let [codec (resolve-codec codec-or-k)]
     (do
       (align-buffer-read (alignment* codec encoding) buffer)
       (from-buffer!* codec encoding buffer))))

(def unspecified-order :endian/unspecified)
(def byte-orders {:endian/little ByteOrder/LITTLE_ENDIAN
                  :endian/big ByteOrder/BIG_ENDIAN
                  :endian/native (ByteOrder/nativeOrder)
                  :endian/network ByteOrder/BIG_ENDIAN
                  :endian/unspecified unspecified-order})

; Byte order spec looks up a keyword in the byte-orders map, or checks to see
; if the value is a ByteOrder.  Otherwise it  returns invalid
(s/def ::byte-order (s/conformer 
                      (let [order? #(and (instance? ByteOrder %) %)]
                        (fn [maybe-order-or-k]
                          (if-let [found-order (get byte-orders maybe-order-or-k)]
                            found-order
                            (or (order? maybe-order-or-k) ::s/invalid?))))))

(s/def ::word-size #{1 2 4 8})

(s/def ::base-encoding (s/keys :req-un [::byte-order ::word-size]))

(def base-encoding {:byte-order :endian/unspecified :word-size 1})

(defn- buffer-op-with-endian [endian buffer-op buffer]
  (if (= unspecified-order endian)
    (buffer-op buffer)
    (let [old-order (.order buffer)]
      (do
        (.order buffer endian)
        (buffer-op buffer)
        (.order buffer old-order)))))

(defn validate-encoding [default-encoding spec encoding]
  "If passed encoding is not nil, then merge it with the base-encoding, and finally conform it
  to the encoder spec.  If there is a problem conforming, then throw an expection.
  If the passed encoding is nil, then just return the base-encoding"
  (if (some? encoding)
    (let [merged-encoding (merge default-encoding encoding)
          enc (s/conform spec merged-encoding)]
      (if (s/invalid? enc)
        (throw (IllegalArgumentException.
                 (format "Unable to conform encoding %s - %s"
                         encoding
                         (s/explain-str spec merged-encoding))))
        enc))
    base-encoding))

(binary-codec.core/def
  ::int8 
  (reify Codec
    (encoder* [_ encoding] (validate-encoding base-encoding ::base-encoding encoding))
    (alignment* [_ encoding] 1)
    (sizeof* [_ _] Byte/BYTES)
    (sizeof* [_ _ _] Byte/BYTES)
    (to-buffer!* [_ _ data buffer] (.put buffer data))
    (from-buffer!* [_ _ buffer] (.get buffer)))
  (make-signed-integral-conformer byte))

(binary-codec.core/def
  ::int16
  (reify Codec
    (encoder* [_ encoding] (validate-encoding base-encoding ::base-encoding encoding))
    (alignment* [_ encoding] (min Short/BYTES (:word-size encoding)))
    (sizeof* [_ encoding] Short/BYTES)
    (sizeof* [_ encoding data] Short/BYTES)
    (to-buffer!* [_ encoding data buffer]
      (buffer-op-with-endian (:byte-order encoding) #(.putShort % data) buffer))
    (from-buffer!* [_ encoding buffer]
      (buffer-op-with-endian (:byte-order encoding) #(.getShort %) buffer)))
  (make-signed-integral-conformer short))

(binary-codec.core/def 
  ::int32
  (reify Codec
    (encoder* [_ encoding] (validate-encoding base-encoding ::base-encoding encoding))
    (alignment* [_ encoding] (min Integer/BYTES (:word-size encoding)))
    (sizeof* [_ encoding] Integer/BYTES)
    (sizeof* [_ encoding data] Integer/BYTES)
    (to-buffer!* [_ encoding data buffer]
      (buffer-op-with-endian (:byte-order encoding) #(.putInt % data) buffer))
    (from-buffer!* [_ encoding buffer]
      (buffer-op-with-endian (:byte-order encoding) #(.getInt %) buffer)))
  (make-signed-integral-conformer int))

(binary-codec.core/def 
  ::int64
  (reify Codec
    (encoder* [_ encoding] (validate-encoding base-encoding ::base-encoding encoding))
    (alignment* [_ encoding] (min Long/BYTES (:word-size encoding)))
    (sizeof* [_ encoding] Long/BYTES)
    (sizeof* [_ encoding data] Long/BYTES)
    (to-buffer!* [_ encoding data buffer]
      (buffer-op-with-endian (:byte-order encoding) #(.putLong % data) buffer))
    (from-buffer!* [_ encoding buffer]
      (buffer-op-with-endian (:byte-order encoding) #(.getLong %) buffer)))
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

(defn make-fixed-array-codec [count-of c]
  (reify Codec
    (encoder* [_ encoding] (encoder c encoding))
    (alignment* [_ encoding] (unchecked-alignment c encoding))
    (sizeof* [_ encoding] 
      (if-let [size (unchecked-sizeof c encoding)]
        (* count-of (+ size (alignment-padding (unchecked-alignment c encoding) size)))
        nil))
    (sizeof* [_ encoding data]
      (if (= count-of (count data))
        (reduce (fn [accum elem]
                  (if-let [size (unchecked-sizeof c encoding elem)]
                    (+ accum size (alignment-padding (unchecked-alignment c encoding) accum))
                    (reduced nil)))
                0
                data)
        nil))
    (to-buffer!* [_ encoding data buffer] 
      (doseq [elem data]
        (unchecked-to-buffer! c encoding elem buffer))
      buffer)
    (from-buffer!* [this encoding buffer]
      (into [] (doall (map #(unchecked-from-buffer! %1 encoding buffer) (repeat count-of c)))))))


;;;;BIG TODO
;;;;Figure this varaible array out
(defn make-variable-array-codec [c]
  (reify Codec
    (encoder* [_ encoding] (encoder c encoding))
    (alignment* [_ encoding] (unchecked-alignment c encoding))
    (sizeof* [_ encoding] nil
      (if-let [count-of (:count encoding)]
        (if-let [size (unchecked-sizeof c encoding)]
          (* count-of (+ size (alignment-padding (unchecked-alignment c encoding) size)))
          nil)
        (throw (IllegalArgumentException. 
                 "Unable to determine the size of the array.  Either pass some data, or use the :count field of the encoding"))))
    (sizeof* [_ encoding data]
      (reduce (fn [accum elem]
                (if-let [size (unchecked-sizeof c encoding elem)]
                  (+ accum size (alignment-padding (unchecked-alignment c encoding) accum))
                  (reduced nil)))
              0
              data))
      (to-buffer!* [_ encoding data buffer]
        (doseq [elem data]
          (unchecked-to-buffer! c encoding elem buffer))
        buffer)
    (from-buffer!* [this encoding buffer]
      (if-let [count-of (:count encoding)]
        (into [] (doall (map #(unchecked-from-buffer! %1 encoding buffer) (repeat count-of c))))
        (throw (IllegalArgumentException. 
                 "Unable to determine the size of the array.  Use the :count field of the encoding"))))))


(defmacro array [codec & {:keys [kind count min-count max-count distinct into]
                          :or {kind nil count nil min-count nil max-count nil distinct nil}}]
  (let [spec (codec-spec codec)
        coll-spec `(s/coll-of ~spec
                             :kind ~kind 
                             :count ~count 
                             :min-count ~min-count
                             :max-count ~max-count
                             :distinct ~distinct
                             :into [])
        c `(if (number? ~count)
             (make-fixed-array-codec ~count ~codec)
             (make-variable-array-codec ~codec))]
    `(with-meta ~c {:spec ~coll-spec})))

(defn tuple-impl [codecs specs]
    (with-meta 
       (reify Codec
         (encoder* [_ encoding]
           (if (some? encoding)
             (let [tuple-encoding (merge base-encoding (dissoc encoding :index-map))
                   index-map (:index-map encoding)]
               ;Map over the codecs.  If there the index of the codec is in the index-map
               ;merge it with the global encoding
               (map (fn [c index]
                      (encoder c (merge tuple-encoding (get index-map index))))
                      codecs
                      (range)))
             (map encoder codecs)))
         (alignment* [_ encoding] (apply max (map unchecked-alignment codecs encoding)))
         (sizeof* [_ encoding]
           (reduce (fn [accum [c enc]]
                     (if-let [size (unchecked-sizeof c enc)]
                       (+ accum size (alignment-padding (unchecked-alignment c enc) accum))
                       (reduced nil)))
                   0
                   (map vector codecs encoding)))
         (sizeof* [_ encoding data]
           (reduce (fn [accum [c enc elem]]
                     (if-let [size (unchecked-sizeof c enc elem)]
                       (+ accum size (alignment-padding (unchecked-alignment c enc) accum))
                       (reduced nil)))
                   0
                   (map vector codecs encoding (lazy-pad data nil))))
         (to-buffer!* [_ encoding data buffer]
           (doseq [[codec enc elem] (map vector codecs encoding data)]
             (unchecked-to-buffer! codec enc elem buffer))
           buffer)
         (from-buffer!* [_ encoding buffer]
           (into [] (doall (map #(unchecked-from-buffer! %1 %2 buffer) codecs encoding)))))
       {:spec specs}))

(defmacro tuple 
  "Given a list of codecs this will generate a codec and spec that takes a tuple (e.g. vector)

  When a tuple is encoded, the encoding map will be passed to every codec.  There is also a 
  special key :binary-codec.core/index-map which is a map of indexes to encodings

  For example:
  (def tup (codec/tuple ::codec/uint8 ::codec/uint16 ::codec/uint8))
  (alignmnet tup {:word-size 4 
               :byte-order ::encoding/endian/little
               :index-map {1 {:byte-order ::encoding/endian/big}
                           2 {:byte-order ::encoding/endian/native}})

  Would encode the tuple with a word alignement of 4.  
  Element 0 would be encoded in little
  Element 1 would be in big endian
  Element 2 would be in network endian"
  [& codecs]
  (let [specs (mapv codec-spec codecs)
        tuple-spec `(s/tuple ~@specs)]
    `(tuple-impl [~@codecs] ~tuple-spec)))


;; A fully qualified keyword is both a keyowrd and has a namespace
(s/def ::fully-qualified-keyword (s/and keyword
                                        namespace))

;; A field is made up of either a tuple of [qualified-keyword raw-keyword]
;; or it is simply a fully-qualfied keyword.
;; This spec is used to detemine which type is being used
(s/def ::field-type (s/or :destructured (s/tuple #{:req :req-un} ::fully-qualified-keyword)
                          :raw ::fully-qualified-keyword))

(s/def ::field-type-conformer (s/conformer
                                (fn [k-or-tuple]
                                  (let [[k-type k-value] (s/conform ::field-type k-or-tuple)]
                                    (case k-type
                                      :destructured k-value
                                      :raw [:req k-value])))))

(s/def ::field-is-keyword (comp keyword? second))
(s/def ::keyword-is-registered (comp specified-codec? second))

;; A field must be a tuple that has a fully qualified keyword for spec, and then a possibly unqualified field
(s/def ::field (s/and ::field-type-conformer
                      ::field-is-keyword 
                      ::keyword-is-registered))


(defn unqualified 
  "Produces a tuple of the passed keyword and an unqualified version of the keyword
  This function can be used to force a struct to use the unqualified keyword for a field"
  [k]
  (if (s/valid? ::fully-qualified-keyword k)
    [:req-un k]
    (throw (IllegalArgumentException. (str "Passed keyword is not fully qualified " k)))))
  
(defn qualified 
  "Produces a tuple of fully qualified keyword followed by the same fully qualified keyword
  Usually you do not need this function, and can instead pass a single keyword directly to a struct"
  [k]
  (if (s/valid? ::fully-qualified-keyword k)
    [:req k]
    (throw (IllegalArgumentException. (str "Passed keyword is not fully qualified " k)))))

(s/def ::fields (s/coll-of ::field :into []))

(defn form-keys [fields]
  (let [extract-fields (fn [k flds]
                         (mapv second (filter #(= k (first %)) flds)))
        req (extract-fields :req fields)
        req-un (extract-fields :req-un fields)
        spec `(s/keys :req ~req :req-un ~req-un)]
    spec))


(defn struct-impl [fields spec]
  (let[codec-keys (map second fields)
       data-keys (map
                    (fn [[qual k]] 
                      (if (= :req-un qual) 
                        (keyword (name k)) 
                        k))
                    fields)]
    (with-meta 
      (reify Codec
        (encoder* [_ encoding]
           (if (some? encoding)
             (let [struct-encoding (merge base-encoding (dissoc encoding :key-map))
                   key-map (:key-map encoding)]
               ;Map over the codecs.  If there the index of the codec is in the index-map
               ;merge it with the global encoding
               (into {} (map (fn [ck]
                               [ck (encoder ck (merge struct-encoding (get key-map ck)))])
                             codec-keys)))
             (into {} (map #(vector % (encoder %)) codec-keys))))
        (alignment* [_ encoding] (apply max (map #(unchecked-alignment % (get encoding %)) codec-keys)))
        (sizeof* [_ encoding]
          (reduce (fn [accum ck]
                    (let [enc (get encoding ck)
                          size (unchecked-sizeof ck enc)]
                      (if (some? size)
                        (+ accum size (alignment-padding (unchecked-alignment ck enc) accum))
                        (reduced nil))))
                  0
                  codec-keys))
        (sizeof* [_ encoding data]
          (reduce (fn [accum [ck dk]]
                    (let [enc (get encoding ck)
                          elem (get data dk)
                          size (unchecked-sizeof ck enc elem)]
                      (if (some? size)
                        (+ accum size (alignment-padding (unchecked-alignment ck enc) accum))
                        (reduced nil))))
                  0
                  (map vector codec-keys data-keys)))
        (to-buffer!* [_ encoding data buffer] 
           (doseq [[ck dk] (map vector codec-keys data-keys)]
             (unchecked-to-buffer! ck (get encoding ck) (get data dk) buffer))
           buffer)
        (from-buffer!* [_ encoding buffer]
          (into {} (doall (map (fn [ck dk]
                                 (let [enc (get encoding ck)
                                       val (unchecked-from-buffer! ck enc buffer)]
                                   [dk val]))
                               codec-keys data-keys)))))
      {:spec spec})))


(defmacro struct 
  "Creates a strucutre based on a list of fields passed in
  A field can be either a fully qualified keyword registiered with spec,
  or it can be a keyword that has been destructured using unqualified-field.

  When using the codec fields are evaluated in order.  When using the struct
  all keywords are used as :req arguments to s/keys, while destructured keywords
  are used as :req-un arguments
  
  When encoding a struct, there is a special key that can be included 'key-map'. If there is 
  a key in the key-map that matches one of the codecs key, then that value will be merged in
  with the encoding
  
  For example if there is a struct foo, that has codec ::bar ::baz ::bah, then in the following example
  (def foo foo [::bar ::baz ::plo]) 
  (alignment foo {:word-size 4 :key-map {::bar {:word-size 8}
                                         ::baz {:byte-order :endian/big})

  ::baz and ::plo would have a word size of 4
  ::bar would have a word size of 8
  ::baz would have a big endian encoding"
  [& fields]
  (let [proc-fields `[~@fields]
        flds `(if (s/valid? ::fields ~proc-fields)
               (s/conform ::fields ~proc-fields)
               (throw 
                 (IllegalArgumentException.
                   (str "Unable to conform fields \n" (s/explain-str ::fields ~proc-fields)))))
        spec `(form-keys ~flds)]
          `(struct-impl ~flds (eval ~spec))))

; (defn align 
;   "Similar to the __align pragma in C, this can be used to add extra alignment padding
;   to a single codec.  Will not affect the internal structure of the codec.  To do that, you
;   should use the encode function"
;   [align-to codec]
;   (reify Codec
;     (encode* [this encoding] 
;       ;Return codec with this's current spec as metadata
;       (with-meta 
;         (align align-to (encode codec encoding))
;         (assoc (meta encoding) :spec (codec-spec this))))
;     (encoded?* [this] (encoded? codec))
;     (alignment* [_] 
;       (let [old-alignment (alignment codec)
;             new-alignment (max align-to old-alignment)]
;         (if (= 0 (mod new-alignment old-alignment))
;           new-alignment
;           (throw (IllegalArgumentException.
;                    (format "cannot align to new alignment (%d) because it is not an even multiple of the old alignment (%d)" new-alignment old-alignment))))))
                                               

;     (sizeof* [_] (sizeof codec))
;     (sizeof* [_ data] (sizeof codec data))
;     (to-buffer!* [_ data buffer] (to-buffer! codec data buffer))
;     (from-buffer!* [_ buffer] (from-buffer! codec buffer))))

; (defn unaligned 
;   "Removes an alignement values fronm a specific codec.  Will not affect the internal structure
;   of the codec. To do that, you should use the encode function"
;   [codec]
;   (reify Codec
;     (encode* [this encoding] 
;       ;Return codec with this's current spec as metadata
;       (with-meta 
;         (unaligned (encode codec encoding)) 
;         (assoc (meta encoding) :spec (codec-spec this))))
;     (encoded?* [this] (encoded? codec))
;     (alignment* [_] 0)
;     (sizeof* [_] (sizeof codec))
;     (sizeof* [_ data] (sizeof codec data))
;     (to-buffer!* [_ data buffer] (to-buffer! codec data buffer))
;     (from-buffer!* [_ buffer] (from-buffer! codec buffer))))

