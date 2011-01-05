(ns bin-ed.test.core
  (:use [bin-ed.core] :reload)
  (:use [clojure.test]))

(deftest test-file-to-bytes
  (is (seq? (file-to-bytes "data/a.out"))))

(deftest test-file-reading-and-writing
  (let [bytes (range 240)
        path "data/test.out"]
    (is (= bytes
           (do (bytes-to-file path bytes)
               (bytes-from-file path))))))

(deftest test-num-and-byte-conversions
  (doseq [n [0 1 3 17 255 256 278 2414]]
    (doseq [m [2 3 4]]
      (is (= n (bytes-to-num (num-to-bytes n m)))))))
