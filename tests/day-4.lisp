(defpackage :day-4-tests
  (:use :cl :fiveam
        :utils :tests :day-4))

(in-package :day-4-tests)

(def-suite test-day-4
  :description "Day 2 tests"
  :in tests:all-tests)

(in-suite test-day-4)

(test day-4.1-example
  (let ((input (aoc/read-matrix 4 :example)))
    (is (= (day-4:part-1 input) 18))))

(test day-4.1-real
  (let ((input (aoc/read-matrix 4 :real)))
    (is (= (day-4:part-1 input) 2530))))

(test day-4.2-example
  (let ((input (aoc/read-matrix 4 :example)))
    (is (= (day-4:part-2 input) 9))))

(test day-4.2-real
  (let ((input (aoc/read-matrix 4 :real)))
    (is (= (day-4:part-2 input) 1921))))
