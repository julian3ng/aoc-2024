(defpackage :day-10
  (:use :cl :april :ppcre :utils)
  (:export :part-1 :part-2))

(in-package :day-10)

(defun neighbors-4 (p bounds)
  (destructuring-bind (my mx) bounds  
    (destructuring-bind (y x) p
      (labels ((in-bounds (y x)
                 (and (<= 0 y (1- my))
                      (<= 0 x (1- mx)))))
        (loop for (i j) in '((-1 0) (0 1) (1 0) (0 -1))
              when (in-bounds (+ y i) (+ x j))
                collect (list (+ y i) (+ x j)))))))

(defun dfs (board start)
  (let ((nines 0)
        (visited (make-hash-table :test 'equalp)))
    (destructuring-bind (y x) start
      (labels (()))
      )))

(defun part-1 (input) input)
(defun part-2 (input) input)

(aoc/read-matrix 10 :example)
