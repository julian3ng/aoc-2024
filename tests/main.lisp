(defpackage :tests
  (:use :cl :fiveam)
  (:export :all-tests))

(in-package :tests)

(fiveam:def-suite all-tests
  :description "Root test suite")
