(defpackage :day-5-tests
  (:use :cl :fiveam
        :utils :tests :day-5))

(in-package :day-5-tests)

(def-suite test-day-5
  :description "Day 5 tests"
  :in tests:all-tests)

(in-suite test-day-5)

(test day-5.1-example
  (let ((input (aoc/read-input 5 :example)))
    (is (= (day-5:part-1 input) 143))))

(test day-5.1-real
  (let ((input (aoc/read-input 5 :real)))
    (is (= (day-5:part-1 input) 5452))))

(test day-5.2-example
  (let ((input (aoc/read-input 5 :example)))
    (is (= (day-5:part-2 input) 123))))

(test day-5.2-real
  (let ((input (aoc/read-input 5 :real)))
    (is (= (day-5:part-2 input) 4598))))
