(defpackage :day-1-tests
  (:use :cl :fiveam :tests :day-1))

(in-package :day-1-tests)

(fiveam:def-suite test-day-1
  :description "Day 1 tests"
  :in tests:all-tests)

(fiveam:in-suite test-day-1)

(fiveam:test day-1.1-example
  (let ((lines (uiop:read-file-lines "input/day-1/example.txt")))
    (fiveam:is (=  (length (day-1:part-1 lines)) 4))))


(fiveam:test day-1.1-real
  (let ((lines (uiop:read-file-lines "input/day-1/real.txt")))
    (fiveam:is (=  (length (day-1:part-1 lines)) 4))))
