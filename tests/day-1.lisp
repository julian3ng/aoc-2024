(defpackage :day-1-tests
  (:use :cl :fiveam
        :utils :tests :day-1))

(in-package :day-1-tests)

(fiveam:def-suite test-day-1
  :description "Day 1 tests"
  :in tests:all-tests)

(fiveam:in-suite test-day-1)

(fiveam:test day-1.1-example
  (let ((input (utils:aoc/read-int-array 1 :example)))
    (fiveam:is (= (day-1:part-1 input) 11))))

(fiveam:test day-1.1-real
  (let ((input (utils:aoc/read-int-array 1 :real)))
    (fiveam:is (= (day-1:part-1 input)) 2113135)))

(fiveam:test day-1.2-example
  (let ((input (utils:aoc/read-int-array 1 :example)))
    (fiveam:is (= (day-1:part-2 input) 31))))

(fiveam:test day-1.2-real
  (let ((input (utils:aoc/read-int-array 1 :real)))
    (fiveam:is (= (day-1:part-2 input) 19097157))))
