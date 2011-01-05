(ns bin-ed.test.core
  (:use [bin-ed.core] :reload)
  (:use [clojure.test]))

(deftest test-file-to-bytes
  (is (seq? (file-to-bytes "data/a.out"))))
