(defpackage :day-3
  (:use :cl :cl-ppcre :utils)
  (:export :part-1 :part-2))

(in-package :day-3)

(proclaim '(optimize (debug 3) (speed 0)))

(defun part-1 (input)
  (let ((sum 0))
    (do-register-groups (a b) ("mul\\((-?\\d+),(-?\\d+)\\)" input)
      (incf sum (apply #'* (mapcar #'parse-integer (list a b)))))
    sum))

;; (part-1 (aoc/read-input 3 :real)) ; => 162813399 (28 bits, #x9B455D7)

(defun part-2 (input)
  (let ((sum 0)
        (state 'enabled))
    (do-register-groups (on off mul a b) ("(?:(do\\(\\))|(don't\\(\\))|(mul\\((-?\\d+),(-?\\d+)\\)))" input)
      (when (and (eq state 'enabled) off)
        (setf state 'disabled))

      (when (and (eq state 'disabled) on)
        (setf state 'enabled))

      (when (and (eq state 'enabled) mul)
        (incf sum (apply #'* (mapcar #'parse-integer (list a b))))))
    sum))

;; (part-2 (aoc/read-input 3 :real)) ; => 53783319 (26 bits, #x334AB17)


