(defpackage :day-3-tests
  (:use :cl :fiveam
        :utils :tests :day-3))

(in-package :day-3-tests)

(def-suite test-day-3
  :description "Day 3 tests"
  :in tests:all-tests)

(in-suite test-day-3)

(test day-3.1-example
  (let ((input (aoc/read-input 3 :example)))
    (is (= (day-3:part-1 input) 161))))

(test day-3.1-real
  (let ((input (aoc/read-input 3 :real)))
    (is (= (day-3:part-1 input) 162813399))))

(test day-3.2-example
  (let ((input "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"))
    (is (= (day-3:part-2 input) 48))))

(test day-3.2-real
  (let ((input (aoc/read-input 3 :real)))
    (is (= (day-3:part-2 input) 53783319))))
