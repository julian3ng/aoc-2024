(defpackage :day-6-tests
  (:use :cl :fiveam
        :utils :tests :day-6))

(in-package :day-6-tests)

(def-suite test-day-6
  :description "Day 6 tests"
  :in tests:all-tests)

(in-suite test-day-6)

(test day-6.1-example
  (let ((input (aoc/read-matrix 6 :example)))
    (is (= (day-6:part-1 input) 41))))

(test day-6.1-real
  (let ((input (aoc/read-matrix 6 :real)))
    (is (= (day-6:part-1 input) 4973))))

(test day-6.2-example
  (let ((input (aoc/read-matrix 6 :example)))
    (is (= (day-6:part-2 input) 6))))

(test day-6.2-real
  (let ((input (aoc/read-matrix 6 :real)))
    (is (= (day-6:part-2 input) 1482))))
