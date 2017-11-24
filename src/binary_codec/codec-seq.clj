(ns binary-codec.core
  (:import (java.nio ByteBuffer
                     ByteOrder))
  (:require [clojure.spec.alpha :as s]))

; There is a bit of a conflict here
; Clojure works so hard to think in terms of sequences,
; but all of the java binary stuff doesn't quite work as a sequence

;-----------------
; We can split things up in a different direction.  
; Think in terms of primitives and sequences
;
; Step 1 of encoding is to convert the data to 
; a sequence of primitives.  Then you can map a sequence
; of primitives to a byte array
;

(defn 
