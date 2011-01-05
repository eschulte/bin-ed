(ns bin-ed.core
  (:use clojure.contrib.math)
  (:import (java.io FileOutputStream FileInputStream FileReader File)
           (java.nio Buffer ByteBuffer CharBuffer)))


;;; utility functions
(defn file-to-bytes [path]
  (let [stream (FileInputStream. path)
        ary (byte-array (.available stream))]
    (.read stream ary)
    (seq ary)))

(defn bytes-to-file [path bytes]
  (let [stream (FileOutputStream. path)]
    (.write stream (byte-array bytes))))

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
        (throw (Error. "Insufficient bytes for template."))
        (let [[k v] (first tmpl)
              [n f] (eval (concat ['case v] binary-types [v]))]
          (recur (drop n bytes)
                 (rest tmpl)
                 (concat [(f (bytes-to-num (take n bytes))) k]
                         out)))))))

(defn size-of "Return size of template in bytes." [template]
  (reduce + (map (fn [[_ v]] (first (eval (concat ['case v] binary-types [v]))))
                 (partition 2 template))))

(comment
  ;; example: parse the header of an elf file
  (let [a-out (file-to-bytes "data/a.out")
        head-ident-tmpl [:mag0       :byte
                         :mag1       :char
                         :mag2       :char
                         :mag3       :char
                         :class      :byte
                         :data       :byte
                         :ei_version :byte
                         :osabi      :byte
                         :abiversion :byte
                         :padding    [7 (fn [_] nil)]]
        head-ident (parse a-out head-ident-tmpl)
        ;; address and offset sizes vary on 32 and 64 bit machines
        size (case (int (:class (apply hash-map head-ident)))
                   1 :uint32 2 :uint64)
        head-rest-tmpl [:type        :uint16
                        :machine     :uint16
                        :elf_version :uint32
                        :entry       size
                        :phoff       size
                        :shoff       size
                        :flags       :uint32
                        :ehsize      :uint16
                        :phentsize   :uint16
                        :phnum       :uint16
                        :shentsize   :uint16
                        :shnum       :uint16
                        :shstrndx    :uint16]]
    (concat
     head-ident
     (parse (drop (size-of head-ident-tmpl) a-out) head-rest-tmpl)))
  )
