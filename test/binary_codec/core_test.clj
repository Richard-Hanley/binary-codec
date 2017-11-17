(ns binary-codec.core-test
  (:import (java.nio ByteBuffer
                     ByteOrder))
  (:require [clojure.test :refer :all]
            [binary-codec.core :as codec :refer :all]))

(deftest test-numerical-sizes
  (is (= 1 (codec/sizeof ::codec/int8)))
  (is (= 2 (codec/sizeof ::codec/int16)))
  (is (= 4 (codec/sizeof ::codec/int32)))
  (is (= 8 (codec/sizeof ::codec/int64))))

(deftest test-numerical-alignment
  (testing "Base Encoding (no alignment)"
    (is (= 0 (codec/alignment ::codec/int8)))
    (is (= 0 (codec/alignment ::codec/int16)))
    (is (= 0 (codec/alignment ::codec/int32)))
    (is (= 0 (codec/alignment ::codec/int64))))
  (testing "Alignment specified (1-byte)"
    (is (= 0 (codec/alignment ::codec/int8 {::codec/word-size 1})))
    (is (= 1 (codec/alignment ::codec/int16 {::codec/word-size 1})))
    (is (= 1 (codec/alignment ::codec/int32 {::codec/word-size 1})))
    (is (= 1 (codec/alignment ::codec/int64 {::codec/word-size 1}))))
  (testing "Alignment specified (2-byte)"
    (is (= 0 (codec/alignment ::codec/int8 {::codec/word-size 2})))
    (is (= 2 (codec/alignment ::codec/int16 {::codec/word-size 2})))
    (is (= 2 (codec/alignment ::codec/int32 {::codec/word-size 2})))
    (is (= 2 (codec/alignment ::codec/int64 {::codec/word-size 2}))))
  (testing "Alignment specified (4-byte)"
    (is (= 0 (codec/alignment ::codec/int8 {::codec/word-size 4})))
    (is (= 2 (codec/alignment ::codec/int16 {::codec/word-size 4})))
    (is (= 4 (codec/alignment ::codec/int32 {::codec/word-size 4})))
    (is (= 4 (codec/alignment ::codec/int64 {::codec/word-size 4}))))
  (testing "Alignment specified (8-byte)"
    (is (= 0 (codec/alignment ::codec/int8 {::codec/word-size 8})))
    (is (= 2 (codec/alignment ::codec/int16 {::codec/word-size 8})))
    (is (= 4 (codec/alignment ::codec/int32 {::codec/word-size 8})))
    (is (= 8 (codec/alignment ::codec/int64 {::codec/word-size 8})))))

(deftest test-numerical-put
  (testing "Test Byte"
    (testing "Little Endian"
      (let [buffer (.flip (to-buffer! ::codec/int8 
                                      (byte 15) 
                                      (.order (ByteBuffer/allocate 10) ByteOrder/LITTLE_ENDIAN)))]
        (is (= 15 (from-buffer! ::codec/int8 buffer)))))
    (testing "Big Endian"
      (let [buffer (.flip (to-buffer! ::codec/int8 
                                      (byte 15) 
                                      (.order (ByteBuffer/allocate 10) ByteOrder/BIG_ENDIAN)))]
        (is (= 15 (from-buffer! ::codec/int8 buffer))))))
  (testing "Test Short"
    (testing "Little Endian"
      (let [buffer (.flip (to-buffer! ::codec/int16 
                                      (short 0x7F4) 
                                      (.order (ByteBuffer/allocate 10) ByteOrder/LITTLE_ENDIAN)))]
        (is (= 0x7F4 (from-buffer! ::codec/int16 buffer)))))
    (testing "Big Endian"
      (let [buffer (.flip (to-buffer! ::codec/int16 
                                      (short 0x7F4) 
                                      (.order (ByteBuffer/allocate 10) ByteOrder/BIG_ENDIAN)))]
        (is (= 0x7F4 (from-buffer! ::codec/int16 buffer))))))
  (testing "Test Integer"
    (testing "Little Endian"
      (let [buffer (.flip (to-buffer! ::codec/int32 
                                      (int 0x7890BEEF) 
                                      (.order (ByteBuffer/allocate 10) ByteOrder/LITTLE_ENDIAN)))]
        (is (= 0x7890BEEF (from-buffer! ::codec/int32 buffer)))))
    (testing "Big Endian"
      (let [buffer (.flip (to-buffer! ::codec/int32 
                                      (int 0x7890BEEF) 
                                      (.order (ByteBuffer/allocate 10) ByteOrder/BIG_ENDIAN)))]
        (is (= 0x7890BEEF (from-buffer! ::codec/int32 buffer))))))
  (testing "Test Long"
    (testing "Little Endian"
      (let [buffer (.flip (to-buffer! ::codec/int64 
                                      (long 0x31337DEADBEEF) 
                                      (.order (ByteBuffer/allocate 10) ByteOrder/LITTLE_ENDIAN)))]
        (is (= 0x31337DEADBEEF (from-buffer! ::codec/int64 buffer)))))
    (testing "Big Endian"
      (let [buffer (.flip (to-buffer! ::codec/int64 
                                      (long 0x31337DEADBEEF) 
                                      (.order (ByteBuffer/allocate 10) ByteOrder/BIG_ENDIAN)))]
        (is (= 0x31337DEADBEEF (from-buffer! ::codec/int64 buffer)))))))



(deftest test-aligned
  (let [encoding {::codec/word-size 8}
        fill-buffer-with-offset (fn [offset codec value] 
                                  (let [buffer (ByteBuffer/allocate 40)]
                                    (dotimes [n offset] (.put buffer (byte 0)))
                                    (to-buffer! codec encoding value buffer)
                                    (.flip buffer)))]
    (testing "Alignment off by 1"
      (testing "int16"
        (let [buffer (fill-buffer-with-offset 1 ::codec/int16 (short 0x7F4))]
          (is (= 0 (from-buffer! ::codec/int16 encoding buffer)))
          (is (= 0x7F4 (from-buffer! ::codec/int16 encoding buffer)))))
      (testing "int32"
        (let [buffer (fill-buffer-with-offset 1 ::codec/int32 (int 0x7890BEEF))]
          (is (= 0 (from-buffer! ::codec/int32 encoding buffer)))
          (is (= 0x7890BEEF (from-buffer! ::codec/int32 encoding buffer)))))
      (testing "int64"
        (let [buffer (fill-buffer-with-offset 1 ::codec/int64 (long 0x31337DEADBEEF))]
          (is (= 0 (from-buffer! ::codec/int64 encoding buffer)))
          (is (= 0x31337DEADBEEF (from-buffer! ::codec/int64 encoding buffer))))))
    (testing "Alignment off by 2"
      (testing "int32"
        (let [buffer (fill-buffer-with-offset 2 ::codec/int32 (int 0x7890BEEF))]
          (is (= 0 (from-buffer! ::codec/int32 encoding buffer)))
          (is (= 0x7890BEEF (from-buffer! ::codec/int32 encoding buffer)))))
      (testing "int64"
        (let [buffer (fill-buffer-with-offset 2 ::codec/int64 (long 0x31337DEADBEEF))]
          (is (= 0 (from-buffer! ::codec/int64 encoding buffer)))
          (is (= 0x31337DEADBEEF (from-buffer! ::codec/int64 encoding buffer))))))
    (testing "Alignment off by 3"
      (testing "int32"
        (let [buffer (fill-buffer-with-offset 3 ::codec/int32 (int 0x7890BEEF))]
          (is (= 0 (from-buffer! ::codec/int32 encoding buffer)))
          (is (= 0x7890BEEF (from-buffer! ::codec/int32 encoding buffer)))))
      (testing "int64"
        (let [buffer (fill-buffer-with-offset 3 ::codec/int64 (long 0x31337DEADBEEF))]
          (is (= 0 (from-buffer! ::codec/int64 encoding buffer)))
          (is (= 0x31337DEADBEEF (from-buffer! ::codec/int64 encoding buffer))))))
    (testing "Alignment off by 4"
      (testing "int64"
        (let [buffer (fill-buffer-with-offset 4 ::codec/int64 (long 0x31337DEADBEEF))]
          (is (= 0 (from-buffer! ::codec/int64 encoding buffer)))
          (is (= 0x31337DEADBEEF (from-buffer! ::codec/int64 encoding buffer))))))
    (testing "Alignment off by 5"
      (testing "int64"
        (let [buffer (fill-buffer-with-offset 5 ::codec/int64 (long 0x31337DEADBEEF))]
          (is (= 0 (from-buffer! ::codec/int64 encoding buffer)))
          (is (= 0x31337DEADBEEF (from-buffer! ::codec/int64 encoding buffer))))))
    (testing "Alignment off by 6"
      (testing "int64"
        (let [buffer (fill-buffer-with-offset 6 ::codec/int64 (long 0x31337DEADBEEF))]
          (is (= 0 (from-buffer! ::codec/int64 encoding buffer)))
          (is (= 0x31337DEADBEEF (from-buffer! ::codec/int64 encoding buffer))))))
    (testing "Alignment off by 7"
      (testing "int64"
        (let [buffer (fill-buffer-with-offset 7 ::codec/int64 (long 0x31337DEADBEEF))]
          (is (= 0 (from-buffer! ::codec/int64 encoding buffer)))
          (is (= 0x31337DEADBEEF (from-buffer! ::codec/int64 encoding buffer))))))
    ))
