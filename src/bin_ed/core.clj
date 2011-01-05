(ns bin-ed.core
  (:use clojure.contrib.math)
  (:import (java.io FileInputStream FileReader File)
           (java.nio Buffer ByteBuffer CharBuffer)))


;;; utility functions
(defn file-to-bytes [path]
  (let [stream (FileInputStream. path)
        ary (byte-array (.available stream))]
    (.read stream ary)
    (seq ary)))

(defn bytes-to-num "Concat a seq of bytes into a number." [bytes]
  (reduce (fn [a b] (+ (bit-shift-left a 8) b)) 0 (reverse bytes)))

(defn uint16 "Convert an int16 to a uint16" [x]
  (let [int16 (short x)]
    (if (neg? int16) (+ 0x10000 int16) int16)))

(defn uint32 "Convert an int32 to a uint32" [x]
  (let [int32 (int x)]
    (if (neg? int32) (+ 0x100000000 int32) int32)))

(defn uint64 "Convert an int64 to a uint64" [x]
  (let [int64 (int x)]
    (if (neg? int64) (+ 0x10000000000000000 int64) int64)))


;;; public functions
(def binary-types
  [;; identifier [size-in-bytes conversion-function]
   :byte    [1 byte]
   :char    [1 char]
   :int16   [2 short]
   :uint16  [2 uint16]
   :int32   [4 int]
   :uint32  [4 uint32]
   :int64   [8 long]
   :uint64  [8 uint64]
   :float32 [4 float]
   :float64 [8 double]
   ])

(defn parse
  "Parse a series of bits according to an Alister template"
  [bytes template]
  (loop [bytes bytes
         tmpl (partition 2 template)
         out {}]
    (if (empty? tmpl)
      (reverse out)
      (if (empty? bytes)
        (throw "Insufficient bytes for template.")
        (let [[k v] (first tmpl)
              [n f] (eval (concat ['case v] binary-types [v]))]
          (recur (drop n bytes)
                 (rest tmpl)
                 (concat [(f (bytes-to-num (take n bytes))) k]
                         out)))))))

(comment
  (parse (file-to-bytes "data/a.out")
         ;; elf header ident
         [:mag0       :byte
          :mag1       :char
          :mag2       :char
          :mag3       :char
          :class      :byte
          :data       :byte
          :version    :byte
          :osabi      :byte
          :abiversion :byte
          :padding    [7 (fn [_] nil)]]))
