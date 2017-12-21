(ns binary-codec.core-test
  (:import (java.nio ByteBuffer
                     ByteOrder))
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [binary-codec.core :as codec :refer :all]))


(defn fill-buffer-with-offset-value [offset codec encoding value] 
  (let [buffer (ByteBuffer/allocate 40)]
    (dotimes [n offset] (.put buffer (byte 0)))
    (to-buffer! codec encoding value buffer)
    (.flip buffer)))

(defn extract-byte-seq [num-bytes buffer]
  (doall (map (fn [_] (.get buffer)) (take num-bytes (range)))))

(deftest test-byte
  (testing "confomers"
    (testing "signed"
      (testing "zero"
        (is (= Byte (type (s/conform ::codec/int8 0))))
        (is (= 0 (s/conform ::codec/int8 0))))
      (testing "middle value"
        (is (= Byte (type (s/conform ::codec/int8 15))))
        (is (= 15 (s/conform ::codec/int8 15))))
      (testing "max value"
        (is (= Byte (type (s/conform ::codec/int8 127))))
        (is (= 127 (s/conform ::codec/int8 127))))
      (testing "min value"
        (is (= Byte (type (s/conform ::codec/int8 -128))))
        (is (= -128 (s/conform ::codec/int8 -128))))
      (testing "invalid values"
        (is (= ::s/invalid (s/conform ::codec/int8 128)))
        (is (= ::s/invalid (s/conform ::codec/int8 -129)))
        (is (= ::s/invalid (s/conform ::codec/int8 256)))))
    (testing "unsigned"
      (testing "zero"
        (is (= Byte (type (s/conform ::codec/uint8 0))))
        (is (= 0 (s/conform ::codec/uint8 0))))
      (testing "middle value"
        (is (= Byte (type (s/conform ::codec/uint8 15))))
        (is (= 15 (s/conform ::codec/uint8 15))))
      (testing "max value"
        (is (= Byte (type (s/conform ::codec/uint8 255))))
        (is (= -1 (s/conform ::codec/uint8 255))))
      (testing "min value"
        (is (= Byte (type (s/conform ::codec/uint8 -128))))
        (is (= -128 (s/conform ::codec/uint8 -128))))
      (testing "invalid values"
        (is (= ::s/invalid (s/conform ::codec/uint8 -129)))
        (is (= ::s/invalid (s/conform ::codec/uint8 256))))))
  (testing "sizeof"
    (is (= 1 (codec/sizeof ::codec/int8)))
    (is (= 1 (codec/sizeof ::codec/int8 nil (s/conform ::codec/int8 118)))))
  (testing "alignment value"
    (testing "Base Encoding (no alignment)"
      (is (= 1 (codec/alignment ::codec/int8))))
    (testing "Alignment specified (1-byte)"
      (is (= 1 (codec/alignment ::codec/int8 {:word-size 1}))))
    (testing "alignment (codec/encode specified (2-byte)"
      (is (= 1 (codec/alignment ::codec/int8 {:word-size 2}))))
    (testing "alignment (codec/encode specified (4-byte)"
      (is (= 1 (codec/alignment ::codec/int8 {:word-size 4}))))
    (testing "alignment (codec/encode specified (8-byte)"
      (is (= 1 (codec/alignment ::codec/int8 {:word-size 8})))))
  (testing "buffer operations"
    (let [test-value (byte 118)]
      (testing "simple read"
        (is (= test-value (from-buffer! ::codec/int8 (.rewind (.put (ByteBuffer/allocate 20) test-value))))))
      (testing "simple write"
        (is (= test-value (.get (.rewind (to-buffer! ::codec/int8 test-value (ByteBuffer/allocate 20))))))))))

(deftest test-short
  (testing "conformers"
    (testing "signed"
      (testing "zero"
        (is (= Short (type (s/conform ::codec/int16 0))))
        (is (= 0 (s/conform ::codec/int16 0))))
      (testing "middle value"
        (is (= Short (type (s/conform ::codec/int16 15))))
        (is (= 15 (s/conform ::codec/int16 15))))
        (is (= Short (type (s/conform ::codec/int16 752))))
        (is (= 752 (s/conform ::codec/int16 752)))
    (testing "max value"
      (is (= Short (type (s/conform ::codec/int16 32767))))
      (is (= 32767 (s/conform ::codec/int16 32767))))
    (testing "min value"
      (is (= Short (type (s/conform ::codec/int16 -32768))))
      (is (= -32768 (s/conform ::codec/int16 -32768))))
    (testing "invalid values"
      (is (= ::s/invalid (s/conform ::codec/int16 0xFFFF)))
      (is (= ::s/invalid (s/conform ::codec/int16 0x10000)))
      (is (= ::s/invalid (s/conform ::codec/int16 32768)))
      (is (= ::s/invalid (s/conform ::codec/int16 -32769)))))
    (testing "unsigned"
       (testing "zero"
          (is (= Short (type (s/conform ::codec/uint16 0))))
          (is (= 0 (s/conform ::codec/uint16 0))))
        (testing "middle value"
          (is (= Short (type (s/conform ::codec/uint16 15))))
          (is (= 15 (s/conform ::codec/uint16 15)))
          (is (= Short (type (s/conform ::codec/uint16 752))))
          (is (= 752 (s/conform ::codec/uint16 752))))
        (testing "max value"
          (is (= Short (type (s/conform ::codec/uint16 0xFFFF))))
          (is (= -1 (s/conform ::codec/uint16 0xFFFF))))
        (testing "min value"
          (is (= Short (type (s/conform ::codec/uint16 -32768))))
          (is (= -32768 (s/conform ::codec/uint16 -32768))))
        (testing "invalid values"
          (is (= ::s/invalid (s/conform ::codec/uint16 0x10000)))
          (is (= ::s/invalid (s/conform ::codec/uint16 -32769))))))
  (testing "sizeof"
    (is (= 2 (codec/sizeof ::codec/int16)))
    (is (= 2 (codec/sizeof ::codec/int16 nil (short 0x7FD)))))
  (testing "alignment value"
    (testing "Base Encoding (no alignment)"
      (is (= 1 (codec/alignment ::codec/int16))))
    (testing "Alignment specified (1-byte)"
      (is (= 1 (codec/alignment ::codec/int16 {:word-size 1}))))
    (testing "alignment (codec/encode specified (2-byte)"
      (is (= 2 (codec/alignment ::codec/int16 {:word-size 2}))))
    (testing "alignment (codec/encode specified (4-byte)"
      (is (= 2 (codec/alignment ::codec/int16 {:word-size 4}))))
    (testing "alignment (codec/encode specified (8-byte)"
      (is (= 2 (codec/alignment ::codec/int16 {:word-size 8})))))
  (testing "buffer operations"
    (let [test-value (s/conform ::codec/int16 0x7FD)
          test-bytes [(unchecked-byte 0x07) (unchecked-byte 0xFD)]
          aligned-enc {:word-size 8}]
      (testing "simple read"
        (is (= test-value (from-buffer! ::codec/int16 (.rewind (.putShort (ByteBuffer/allocate 20) test-value))))))
      (testing "simple write"
        (is (= test-value (.getShort (.rewind (to-buffer! ::codec/int16 test-value (ByteBuffer/allocate 20)))))))
      (testing "read/write with alignment off by 1"
        (let [buffer (fill-buffer-with-offset-value 1 ::codec/int16 aligned-enc test-value)]
          (is (= 0 (from-buffer! ::codec/int16 aligned-enc buffer)))
          (is (= test-value (from-buffer! ::codec/int16 aligned-enc buffer)))))
      (testing "read/write with alignment off by 2"
        (let [buffer (fill-buffer-with-offset-value 2 ::codec/int16 aligned-enc test-value)]
          (is (= 0 (from-buffer! ::codec/int16 aligned-enc buffer)))
          (is (= test-value (from-buffer! ::codec/int16 aligned-enc buffer)))))
      
      (testing "read/write to little endian buffer without encoding byte order"
        (is (= (reverse test-bytes) (extract-byte-seq 2 (.rewind (to-buffer! ::codec/int16
                                                                             test-value
                                                                             (.order (ByteBuffer/allocate 20) ByteOrder/LITTLE_ENDIAN)))))))
      (testing "read/write little endian buffer with big encoding"
        (is (= test-bytes (extract-byte-seq 2 (.rewind (to-buffer! ::codec/int16 
                                                                   {:byte-order :endian/big}
                                                                   test-value
                                                                   (.order (ByteBuffer/allocate 20) ByteOrder/LITTLE_ENDIAN)))))))
      (testing "read/write little endian buffer with big encoding literal"
        (is (= test-bytes (extract-byte-seq 2 (.rewind (to-buffer! ::codec/int16 
                                                                   {:byte-order ByteOrder/BIG_ENDIAN}
                                                                   test-value
                                                                   (.order (ByteBuffer/allocate 20) ByteOrder/LITTLE_ENDIAN)))))))
      (testing "read/write to big endian buffer without encoding"
        (is (= test-bytes (extract-byte-seq 2 (.rewind (to-buffer! ::codec/int16
                                                                   test-value
                                                                   (.order (ByteBuffer/allocate 20) ByteOrder/BIG_ENDIAN)))))))
      (testing "read/write to big endian buffer with little encoding"
        (is (= (reverse test-bytes) (extract-byte-seq 2 (.rewind (to-buffer! ::codec/int16 
                                                                             {:byte-order :endian/little}
                                                                             test-value
                                                                             (.order (ByteBuffer/allocate 20) ByteOrder/BIG_ENDIAN)))))))
      (testing "read/write to big endian buffer with little encoding literal"
        (is (= (reverse test-bytes) (extract-byte-seq 2 (.rewind (to-buffer! ::codec/int16 
                                                                             {:byte-order ByteOrder/LITTLE_ENDIAN}
                                                                             test-value
                                                                             (.order (ByteBuffer/allocate 20) ByteOrder/BIG_ENDIAN)))))))
)))

(deftest test-integer
  (testing "conformers"
    (testing "unsigned"
      (testing "zero"
          (is (= Integer (type (s/conform ::codec/uint32 0))))
          (is (= 0 (s/conform ::codec/uint32 0))))
      (testing "middle value"
        (is (= Integer (type (s/conform ::codec/uint32 15))))
        (is (= 15 (s/conform ::codec/uint32 15)))
        (is (= Integer (type (s/conform ::codec/uint32 752))))
        (is (= 752 (s/conform ::codec/uint32 752))))
      (testing "max value"
        (is (= Integer (type (s/conform ::codec/uint32 0xFFFFFFFF))))
        (is (= -1 (s/conform ::codec/uint32 0xFFFFFFFF))))
      (testing "min value"
        (is (= Integer (type (s/conform ::codec/uint32 -2147483648))))
        (is (= -2147483648 (s/conform ::codec/uint32 -2147483648))))
      (testing "invalid values"
        (is (= ::s/invalid (s/conform ::codec/uint32 0x100000000)))
        (is (= ::s/invalid (s/conform ::codec/uint32 -2147483649))))))
  (testing "sizeof"
    (is (= 4 (codec/sizeof ::codec/int32)))
    (is (= 4 (codec/sizeof ::codec/int32 nil (int 0x7FD)))))
  (testing "alignment value"
    (testing "Base Encoding (no alignment)"
      (is (= 1 (codec/alignment ::codec/int32))))
    (testing "Alignment specified (1-byte)"
      (is (= 1 (codec/alignment ::codec/int32 {:word-size 1}))))
    (testing "alignment (codec/encode specified (2-byte)"
      (is (= 2 (codec/alignment ::codec/int32 {:word-size 2}))))
    (testing "alignment (codec/encode specified (4-byte)"
      (is (= 4 (codec/alignment ::codec/int32 {:word-size 4}))))
    (testing "alignment (codec/encode specified (8-byte)"
      (is (= 4 (codec/alignment ::codec/int32 {:word-size 8})))))
  (testing "buffer operations"
    (let [test-value (int 0x1337BEEF)
          test-bytes [(unchecked-byte 0x13 ) (unchecked-byte 0x37) (unchecked-byte 0xBE) (unchecked-byte 0xEF)]
          aligned-int32 {:word-size 8}]
      (testing "simple read"
        (is (= test-value 
               (from-buffer! ::codec/int32 (.rewind 
                                             (.putInt 
                                               (ByteBuffer/allocate 20) 
                                               test-value))))))
      (testing "simple write"
        (is (= test-value 
               (.getInt 
                 (.rewind 
                   (to-buffer! ::codec/int32 test-value (ByteBuffer/allocate 20)))))))
      (testing "read/write with alignment off by 1"
        (let [buffer (fill-buffer-with-offset-value 1 ::codec/int32 aligned-int32 test-value)]
          (is (= 0 (from-buffer! ::codec/int32 aligned-int32 buffer)))
          (is (= test-value (from-buffer! ::codec/int32 aligned-int32 buffer)))))
      (testing "read/write with alignment off by 2"
        (let [buffer (fill-buffer-with-offset-value 2 ::codec/int32 aligned-int32 test-value)]
          (is (= 0 (from-buffer! ::codec/int32 aligned-int32 buffer)))
          (is (= test-value (from-buffer! ::codec/int32 aligned-int32 buffer)))))
      (testing "read/write with alignment off by 3"
        (let [buffer (fill-buffer-with-offset-value 3 ::codec/int32 aligned-int32 test-value)]
          (is (= 0 (from-buffer! ::codec/int32 aligned-int32 buffer)))
          (is (= test-value (from-buffer! ::codec/int32 aligned-int32 buffer)))))
      (testing "read/write with alignment off by 4"
        (let [buffer (fill-buffer-with-offset-value 4 ::codec/int32 aligned-int32 test-value)]
          (is (= 0 (from-buffer! ::codec/int32 aligned-int32 buffer)))
          (is (= test-value (from-buffer! ::codec/int32 aligned-int32 buffer)))))
      (testing "read/write to little endian buffer without encoding byte order"
        (is (= (reverse test-bytes) (extract-byte-seq 4 (.rewind (to-buffer! ::codec/int32
                                                                          test-value
                                                                          (.order (ByteBuffer/allocate 20) ByteOrder/LITTLE_ENDIAN)))))))
      (testing "read/write little endian buffer with big encoding"
        (is (= test-bytes (extract-byte-seq 4 (.rewind (to-buffer! ::codec/int32 
                                                                   {:byte-order :endian/big}
                                                                   test-value
                                                                   (.order (ByteBuffer/allocate 20) ByteOrder/LITTLE_ENDIAN)))))))
      (testing "read/write little endian buffer with big encoding literal"
        (is (= test-bytes (extract-byte-seq 4 (.rewind (to-buffer! ::codec/int32 
                                                                   {:byte-order ByteOrder/BIG_ENDIAN}
                                                                   test-value
                                                                   (.order (ByteBuffer/allocate 20) ByteOrder/LITTLE_ENDIAN)))))))
      (testing "read/write to big endian buffer without encoding"
        (is (= test-bytes (extract-byte-seq 4 (.rewind (to-buffer! ::codec/int32
                                                                   test-value
                                                                   (.order (ByteBuffer/allocate 20) ByteOrder/BIG_ENDIAN)))))))
      (testing "read/write to big endian buffer with little encoding"
        (is (= (reverse test-bytes) (extract-byte-seq 4 (.rewind (to-buffer! ::codec/int32 
                                                                             {:byte-order :endian/little}
                                                                             test-value
                                                                             (.order (ByteBuffer/allocate 20) ByteOrder/BIG_ENDIAN)))))))
      (testing "read/write to big endian buffer with little encoding literal"
        (is (= (reverse test-bytes) (extract-byte-seq 4 (.rewind (to-buffer! ::codec/int32 
                                                                             {:byte-order ByteOrder/LITTLE_ENDIAN}
                                                                             test-value
                                                                             (.order (ByteBuffer/allocate 20) ByteOrder/BIG_ENDIAN)))))))
)))

(deftest test-long
  (testing "sizeof"
    (is (= 8 (codec/sizeof ::codec/int64)))
    (is (= 8 (codec/sizeof ::codec/int64 nil (long 0x7FD)))))
  (testing "alignment value"
    (testing "Base Encoding (no alignment)"
      (is (= 1 (codec/alignment ::codec/int64))))
    (testing "Alignment specified (1-byte)"
      (is (= 1 (codec/alignment ::codec/int64 {:word-size 1}))))
    (testing "alignment (codec/encode specified (2-byte)"
      (is (= 2 (codec/alignment ::codec/int64 {:word-size 2}))))
    (testing "alignment (codec/encode specified (4-byte)"
      (is (= 4 (codec/alignment ::codec/int64 {:word-size 4}))))
    (testing "alignment (codec/encode specified (8-byte)"
      (is (= 8 (codec/alignment ::codec/int64 {:word-size 8})))))
  (testing "buffer operations"
    (let [test-value (long 0x13371234DEADBEEF)
          test-bytes [(unchecked-byte 0x13 ) (unchecked-byte 0x37) (unchecked-byte 0x12) 
                      (unchecked-byte 0x34) (unchecked-byte 0xDE) (unchecked-byte 0xAD)
                      (unchecked-byte 0xBE) (unchecked-byte 0xEF)]
          aligned-int64 {:word-size 8}
          little-int64 {:byte-order :endian/little}
          big-int64 {:byte-order :endian/big}]
      (testing "simple read"
        (is (= test-value (from-buffer! ::codec/int64 (.rewind (.putLong (ByteBuffer/allocate 20) test-value))))))
      (testing "simple write"
        (is (= test-value (.getLong (.rewind (to-buffer! ::codec/int64 test-value (ByteBuffer/allocate 20)))))))
      (testing "read/write with alignment off by 1"
        (let [buffer (fill-buffer-with-offset-value 1 ::codec/int64 aligned-int64 test-value)]
          (is (= 0 (from-buffer! ::codec/int64 aligned-int64 buffer)))
          (is (= test-value (from-buffer! ::codec/int64 aligned-int64 buffer)))))
      (testing "read/write with alignment off by 2"
        (let [buffer (fill-buffer-with-offset-value 2 ::codec/int64 aligned-int64 test-value)]
          (is (= 0 (from-buffer! ::codec/int64 aligned-int64 buffer)))
          (is (= test-value (from-buffer! ::codec/int64 aligned-int64 buffer)))))
      (testing "read/write with alignment off by 3"
        (let [buffer (fill-buffer-with-offset-value 3 ::codec/int64 aligned-int64 test-value)]
          (is (= 0 (from-buffer! ::codec/int64 aligned-int64 buffer)))
          (is (= test-value (from-buffer! ::codec/int64 aligned-int64 buffer)))))
      (testing "read/write with alignment off by 4"
        (let [buffer (fill-buffer-with-offset-value 4 ::codec/int64 aligned-int64 test-value)]
          (is (= 0 (from-buffer! ::codec/int64 aligned-int64 buffer)))
          (is (= test-value (from-buffer! ::codec/int64 aligned-int64 buffer)))))
      (testing "read/write with alignment off by 5"
        (let [buffer (fill-buffer-with-offset-value 5 ::codec/int64 aligned-int64 test-value)]
          (is (= 0 (from-buffer! ::codec/int64 aligned-int64 buffer)))
          (is (= test-value (from-buffer! ::codec/int64 aligned-int64 buffer)))))
      (testing "read/write with alignment off by 6"
        (let [buffer (fill-buffer-with-offset-value 6 ::codec/int64 aligned-int64 test-value)]
          (is (= 0 (from-buffer! ::codec/int64 aligned-int64 buffer)))
          (is (= test-value (from-buffer! ::codec/int64 aligned-int64 buffer)))))
      (testing "read/write with alignment off by 7"
        (let [buffer (fill-buffer-with-offset-value 7 ::codec/int64 aligned-int64 test-value)]
          (is (= 0 (from-buffer! ::codec/int64 aligned-int64 buffer)))
          (is (= test-value (from-buffer! ::codec/int64 aligned-int64 buffer)))))
      (testing "read/write with alignment off by 8"
        (let [buffer (fill-buffer-with-offset-value 8 ::codec/int64 aligned-int64 test-value)]
          (is (= 0 (from-buffer! ::codec/int64 aligned-int64 buffer)))
          (is (= test-value (from-buffer! ::codec/int64 aligned-int64 buffer)))))
      (testing "read/write to little endian buffer without encoding byte order"
        (is (= (reverse test-bytes) (extract-byte-seq 8 (.rewind (to-buffer! ::codec/int64
                                                                             test-value
                                                                             (.order (ByteBuffer/allocate 20) ByteOrder/LITTLE_ENDIAN)))))))
      (testing "read/write little endian buffer with big encoding"
        (is (= test-bytes (extract-byte-seq 8 (.rewind (to-buffer! ::codec/int64 
                                                                   {:byte-order :endian/big}
                                                                   test-value
                                                                   (.order (ByteBuffer/allocate 20) ByteOrder/LITTLE_ENDIAN)))))))
      (testing "read/write little endian buffer with big encoding literal"
        (is (= test-bytes (extract-byte-seq 8 (.rewind (to-buffer! ::codec/int64 
                                                                   {:byte-order ByteOrder/BIG_ENDIAN}
                                                                   test-value
                                                                   (.order (ByteBuffer/allocate 20) ByteOrder/LITTLE_ENDIAN)))))))
      (testing "read/write to big endian buffer without encoding"
        (is (= test-bytes (extract-byte-seq 8 (.rewind (to-buffer! ::codec/int64
                                                                   test-value
                                                                   (.order (ByteBuffer/allocate 20) ByteOrder/BIG_ENDIAN)))))))
      (testing "read/write to big endian buffer with little encoding"
        (is (= (reverse test-bytes) (extract-byte-seq 8 (.rewind (to-buffer! ::codec/int64 
                                                                             {:byte-order :endian/little}
                                                                             test-value
                                                                             (.order (ByteBuffer/allocate 20) ByteOrder/BIG_ENDIAN)))))))
      (testing "read/write to big endian buffer with little encoding literal"
        (is (= (reverse test-bytes) (extract-byte-seq 8 (.rewind (to-buffer! ::codec/int64 
                                                                             {:byte-order ByteOrder/LITTLE_ENDIAN}
                                                                             test-value
                                                                             (.order (ByteBuffer/allocate 20) ByteOrder/BIG_ENDIAN)))))))
)))


; (deftest test-forced-alignmnet
;   (testing "force alignment to 8"
;     (is (= 8 (codec/alignment (codec/align 8 (codec/encode ::codec/int64 {:word-size 8}))))))
;   (testing "force alignment to less than primitive size"
;     (is (= 8 (codec/alignment (codec/align 4 (codec/encode ::codec/int64 {:word-size 8}))))))
;   (testing "force alignment to larger than primitive size"
;     (is (= 16 (codec/alignment (codec/align 16 (codec/encode ::codec/int64 {:word-size 8}))))))
;   (testing "alignment is not even multiple of base alignment"
;     (is (thrown? IllegalArgumentException (codec/alignment (codec/align 15 (codec/encode ::codec/int64 {:word-size 8}))))))
;   (testing "alignment is maintained after re-encoding"
;     (is (= 16 (codec/alignment (codec/encode (codec/align 16 ::codec/int64) {:word-size 8})))))
;   (testing "unaligned"
;     (is (= 0 (codec/alignment (codec/unaligned (codec/encode ::codec/int64 {:word-size 8})))))))

(codec/def ::tfoo (codec/tuple ::codec/int8 ::codec/int64 ::codec/int16))

(deftest test-tuple
  (testing "conformance"
    (testing "valid data" (is (s/valid? ::tfoo [25 12324 754])))
    (testing "data out of range" (is (not (s/valid? ::tfoo [278 12324 754]))))
    (testing "not enough arguments" (is (not (s/valid? ::tfoo [25 12324]))))
    (testing "too many arguments" (is (not (s/valid? ::tfoo [25 12324 754 12 9])))))
  (testing "alignment"
    (testing "unaligned" (is (= 11 (sizeof ::tfoo))))
    (testing "1 byte" (is (= 1 (codec/alignment ::tfoo {:word-size 1}))))
    (testing "2 byte" (is (= 2 (codec/alignment ::tfoo {:word-size 2}))))
    (testing "4 byte" (is (= 4 (codec/alignment ::tfoo {:word-size 4}))))
    (testing "8 byte" (is (= 8 (codec/alignment ::tfoo {:word-size 8})))))
  (testing "alignment using index-map"
    (testing "2 byte" (is (= 2 (codec/alignment ::tfoo {:index-map {1 {:word-size 2}}}))))
    (testing "4 byte" (is (= 4 (codec/alignment ::tfoo {:index-map {1 {:word-size 4}}})))))
  (testing "sizeof"
    (testing "unaligned" (is (= 11 (sizeof ::tfoo))))
    (testing "1 byte alignment" (is (= 11 (sizeof ::tfoo {:word-size 1}))))
    (testing "2 byte alignment" (is (= 12 (sizeof ::tfoo {:word-size 2}))))
    (testing "4 byte alignment" (is (= 14 (sizeof ::tfoo {:word-size 4}))))
    (testing "8 byte alignment" (is (= 18 (sizeof ::tfoo {:word-size 8})))))
  (testing "buffer writing and reading"
    (let [data (s/conform ::tfoo [25 0x31337DEADBEEF 754])
          buffer (.flip 
                   (to-buffer! ::tfoo data (ByteBuffer/allocate 40)))]
      (is (= data (from-buffer! ::tfoo buffer))))))

(codec/def ::mfoo (codec/struct
                    (codec/def ::bar ::codec/uint8 (s/int-in 12 20))
                    (codec/def ::baz ::codec/int64 odd?)
                    (codec/unqualified (codec/def ::bane ::codec/int16 #{1 2 4 8 754}))))
(deftest test-struct
  (testing "alignment"
    (testing "unaligned" (is (= 11 (sizeof ::mfoo))))
    (testing "1 byte" (is (= 1 (alignment ::mfoo {:word-size 1}))))
    (testing "2 byte" (is (= 2 (alignment ::mfoo {:word-size 2}))))
    (testing "4 byte" (is (= 4 (alignment ::mfoo {:word-size 4}))))
    (testing "8 byte" (is (= 8 (alignment ::mfoo {:word-size 8})))))
  (testing "sizeof"
    (testing "unaligned" (is (= 11 (sizeof ::mfoo))))
    (testing "unaligned" (is (= 11 (sizeof ::mfoo nil {::bar 15 ::baz 1245789 :bane 2}))))
    (testing "1 byte alignment" (is (= 11 (sizeof ::mfoo {:word-size 1}))))
    (testing "2 byte alignment" (is (= 12 (sizeof ::mfoo {:word-size 2}))))
    (testing "4 byte alignment" (is (= 14 (sizeof ::mfoo {:word-size 4}))))
    (testing "8 byte alignment" (is (= 18 (sizeof ::mfoo {:word-size 8})))))
  (testing "conformance"
    (testing "valid values"
      (is (s/valid? ::mfoo {::bar 15 ::baz 1245789 :bane 2}))
      (is (s/valid? ::mfoo {::bar 15 ::baz 1245789 :bane 754})))
    (testing "invalid ::bar"
      (is (not (s/valid? ::mfoo {::bar 11 ::baz 1245789 :bane 2})))
      (is (not (s/valid? ::mfoo {::bar 20 ::baz 1245789 :bane 2}))))
    (testing "invalid baz"
      (is (not (s/valid? ::mfoo {::bar 12 ::baz 4 :bane 2}))))
    (testing "invalid bane"
      (is (not (s/valid? ::mfoo {::bar 12 ::baz 4 :bane 3})))
      (is (not (s/valid? ::mfoo {::bar 12 ::baz 4 :bane 9})))))
  (let [data (s/conform ::mfoo {:bane 754 ::baz 0x31337DEADBEEF ::bar 15})
        buffer (.rewind
                 (to-buffer! ::mfoo data (ByteBuffer/allocate 40)))]
    (is (= data (from-buffer! ::mfoo buffer))))
  (testing "individual key encoding"))

; (codec/def ::base-foo {::length ::codec/uint8 ::type ::codec/uint8})
; (codec/def ::fooa {::length ::codec/uint8 ::type ::codec/uint8 ::a ::codec/uint8})
; (codec/def ::foob {::length ::codec/uint8 ::type ::codec/uint8 ::b ::codec/uint16})
; (codec/def ::fooc {::length ::codec/uint8 ::type ::codec/uint8 ::c ::codec/uint32})
; (codec/def ::food {::length ::codec/uint8 ::type ::codec/uint8 ::d ::codec/int64})

; (def foo-type {1 ::fooa 2 ::foob 3 ::fooc 4 ::food})
; (codec/def ::ufoo (codec/tagged-union ::base-foo (fn [foo] (get foo-type (::type foo)))))

; (deftest test-union
;   (testing "sizeof without data" (is (= nil (codec/sizeof ::ufoo))))
;   (testing "fooa"
;     (let [fooa {::length (byte 3) ::type (byte 1) ::a (byte 17)}
;           buff (to-buffer! ::ufoo fooa (ByteBuffer/allocate 20))
;           data (from-buffer! ::ufoo (.flip buff))]
;       (testing "sizeof" (is (= 3 (codec/sizeof ::ufoo fooa))))
;       (testing "encoding" (is (= fooa data)))))
;   (testing "foob"
;     (let [foob {::length (byte 4) ::type (byte 2) ::b (short 257)}
;           buff (to-buffer! ::ufoo foob (ByteBuffer/allocate 20))
;           data (from-buffer! ::ufoo (.flip buff))]
;       (testing "sizeof" (is (= 4 (codec/sizeof ::ufoo foob))))
;       (testing "encoding" (is (= foob data)))))
;   (testing "fooc"
;     (let [fooc {::length (byte 6) ::type (byte 3) ::c (unchecked-int 0xDEADBEEF)}
;           buff (to-buffer! ::ufoo fooc (ByteBuffer/allocate 20))
;           data (from-buffer! ::ufoo (.flip buff))]
;       (testing "sizeof" (is (= 6 (codec/sizeof ::ufoo fooc))))
;       (testing "encoding" (is (= fooc data)))))
;   (testing "food"
;     (let [food {::length (byte 10) ::type (byte 4) ::d (long 0xDEADBEEF)}
;           buff (to-buffer! ::ufoo food (ByteBuffer/allocate 20))
;           data (from-buffer! ::ufoo (.flip buff))]
;       (testing "sizeof" (is (= 10 (codec/sizeof ::ufoo food))))
;       (testing "encoding" (is (= food data))))))


; (codec/def ::fixed-array (codec/array ::codec/uint16 :count 5))
; (codec/def ::var-array (codec/array ::codec/uint16))
; (codec/def ::bounded-array (codec/array ::codec/uint16 :min-count 3 :max-count 6))

; (deftest test-array
;   (testing "invalid values"
;     (testing "invalid values" (is (s/invalid? (s/conform ::fixed-array [1 2 3 4 "Hello this string is not a number"]))))
;     (testing "variable array invalid values" (is (s/invalid? (s/conform ::var-array [1 2 3 4 "Hello this string is not a number"]))))
;     (testing "wrong length for fixed size" (is (s/invalid? (s/conform ::fixed-array [1 2 3 4]))))
;     (testing "not enough for bounded array" (is (s/invalid? (s/conform ::bounded-array [1 2]))))
;     (testing "too many for bounded array" (is (s/invalid? (s/conform ::bounded-array [1 2 3 4 5 6 7]))))
;     )
;   (testing "valid values"
;     (testing "fixed array" (is (= [1 2 3 4 5] (s/conform ::fixed-array '(1 2 3 4 5)))))
;     (testing "var array " (is (= [1 2] (s/conform ::var-array [1 2]))))
;     (testing "bounded array" (is (= [1 2 3 4] (s/conform ::bounded-array [1 2 3 4]))))))

