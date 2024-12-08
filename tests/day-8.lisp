(defpackage :day-8-tests
  (:use :cl :fiveam
        :utils :tests :day-8))

(in-package :day-8-tests)

(def-suite test-day-8
  :description "Day 8 tests"
  :in tests:all-tests)

(in-suite test-day-8)

(test day-8.1-example
  (let ((input (aoc/read-matrix 8 :example)))
    (is (= (day-8:part-1 input) 14))))

(test day-8.1-real
  (let ((input (aoc/read-matrix 8 :real)))
    (is (= (day-8:part-1 input) 379))))

(test day-8.2-example
  (let ((input (aoc/read-matrix 8 :example)))
    (is (= (day-8:part-2 input) 34))))

(test day-8.2-real
  (let ((input (aoc/read-matrix 8 :real)))
    (is (= (day-8:part-2 input) 1339))))
