(defpackage :day-2-tests
  (:use :cl :fiveam
        :utils :tests :day-2))

(in-package :day-2-tests)

(def-suite test-day-2
  :description "Day 2 tests"
  :in tests:all-tests)

(in-suite test-day-2)

(test day-2.1-example
      (let ((input (aoc/read-int-list-of-lists 2 :example)))
        (is (= (day-2:part-1 input) 2))))

(test day-2.1-real
      (let ((input (aoc/read-int-list-of-lists 2 :real)))
        (is (= (day-2:part-1 input) 442))))

(test day-2.2-example
      (let ((input (aoc/read-int-list-of-lists 2 :example)))
        (is (= (day-2:part-2 input) 4))))

(test day-2.2-real
      (let ((input (aoc/read-int-list-of-lists 2 :real)))
        (is (= (day-2:part-2 input) 493))))
