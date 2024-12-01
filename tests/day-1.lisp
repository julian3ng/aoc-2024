(defpackage :day-1-tests
  (:use :cl :fiveam :tests :day-1))

(in-package :day-1-tests)

(fiveam:def-suite test-day-1
  :description "Day 1 tests"
  :in tests:all-tests)

(fiveam:in-suite test-day-1)

(fiveam:test day-1.1-example
  (let ((lines (uiop:read-file-lines "input/day-1/example.txt")))
    (fiveam:is (= (day-1:part-1 lines) 11))))


(fiveam:test day-1.1-real
  (let ((lines (uiop:read-file-lines "input/day-1/real.txt")))
    (format t "~&---> ~A <---~%" (day-1:part-1 lines))
    ;;(fiveam:is (= (day-1:part-1 lines) 4))
    ))

(fiveam:test day-1.2-example
  (let ((lines (uiop:read-file-lines "input/day-1/example.txt")))
    (fiveam:is (= (day-1:part-2 lines) 31))))


(fiveam:test day-1.2-real
  (let ((lines (uiop:read-file-lines "input/day-1/real.txt")))
    (format t "~&---> ~A <---~%" (day-1:part-2 lines))
    ;;(fiveam:is (= (day-1:part-1 lines) 4))
    ))
