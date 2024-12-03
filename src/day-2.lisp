(defpackage :day-2
  (:use :cl :april :utils)
  (:export :part-1 :part-2))

(in-package :day-2)

(defun safe-part-1 (row)
  (and  (or (apply #'< row) (apply #'> row))
        (every (lambda (x) (<= x 3))
               (loop for i from 1 to (1- (length row)) collect (abs (- (elt row i) (elt row (1- i))))))))

(defun part-1 (input)
  (loop for row in input count (safe-part-1 row)))

(defun cut-at-ix (row i)
  (concatenate 'list (subseq row 0 i) (subseq row (1+ i))))

(defun safe-part-2 (row)
  (loop for i from 0 to (1- (length row))
        when (safe-part-1 (cut-at-ix row i))
          return t))

(defun part-2 (input)
  (loop for row in input count (safe-part-2 row)))
