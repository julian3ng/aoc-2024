(defpackage :day-14
  (:use :cl :ppcre :april :utils)
  (:export :part-1 :part-2))

(in-package :day-14)
(proclaim '(optimize (debug 3) (speed 0)))
(defparameter *bounds* (list 103 101))

(defun new-pos (p v time bound)
  (destructuring-bind (y x) p
    (destructuring-bind (dy dx) v
      (destructuring-bind (my mx) bound
        (list (mod  (+ y (* dy time)) my)
              (mod (+ x (* dx time)) mx))))))

(defun score-quadrants (points bound)
  (let ((q1 0)
        (q2 0)
        (q3 0)
        (q4 0))
    (destructuring-bind (my mx) bound
      (let ((hy (floor my 2))
            (hx (floor mx 2)))
        (loop for (y x) in points do
          (cond
            ((and (< y hy) (< x hx))
             (incf q1))
            ((and (< y hy) (< hx x))
             (incf q2))
            ((and (< hy y) (< x hx))
             (incf q3))
            ((and (< hy y) (< hx x))
             (incf q4))))))
    (* q1 q2 q3 q4)))

(defun get-new-points (input time bounds)
  (loop for (x y dx dy) in input
        collect
        (new-pos (list y x) (list dy dx) time bounds)))

(defun part-1 (input time bounds)
  (score-quadrants (get-new-points input time bounds) bounds))

;; (part-1 (aoc/read-ints-ignoring-rest 14 :example) 100 (list 7 11)) ; => 12 (4 bits, #xC, #o14, #b1100)
;; (part-1 (aoc/read-ints-ignoring-rest 14 :real) 100 (list 103 101)) ; => 226548000 (28 bits, #xD80D920)


(defun step-pos (pv time bound)
  (destructuring-bind (y x dy dx) pv
    (destructuring-bind (my mx) bound
      (setf (first pv) (mod (+ y (* dy time)) my)
            (second pv) (mod (+ x (* dx time)) mx)))))

(defun step-input (input time bounds)
  (loop for pv in input do (step-pos pv time bounds)))


(defun part-2 (input time bounds)
  (declare (optimize (speed 3) (debug 0)))
  (let* ((my (first bounds))
         (mx (second bounds))
         (row-counts (make-array my :initial-element 0))
         (col-counts (make-array mx :initial-element 0))
         (board (make-array bounds :initial-element 0)))
    (labels ((good-counts-p () (and (some (lambda (c) (> c 15)) row-counts)
                                    (some (lambda (c) (> c 15)) col-counts)))
             (clear-counts ()
               (loop for i from 0 to (1- my) do (setf (aref row-counts i) 0))
               (loop for j from 0 to (1- mx) do (setf (aref col-counts j) 0)))
             (count-points ()
               (loop for (y x) in input do
                 (incf (aref row-counts y))
                 (incf (aref col-counts x))))
             (create-board () (loop for (y x) in input do (incf (aref board y x))))
             (clear-board () (loop for i from 0 to (1- my) do
               (loop for j from 0 to (1- mx) do
                 (setf (aref board i j) 0))))
             (print-board ()
               (loop for i from 0 to (1- my) do
                 (loop for j from 0 to (1- mx) do
                   (let ((c (aref board i j)))
                     (if (zerop c) (princ #\.) (princ c))))
                 (princ #\newline))))
      (loop for tt from 0 to time do
        (count-points)
        (when (good-counts-p)
          (create-board)
          (print-board)
          (clear-board)
          (return tt))
        (clear-counts)
        (step-input input 1 bounds)))))

;; (part-2 (loop for (x y dx dy) in (aoc/read-ints-ignoring-rest 14 :real)
;;               collect (list y x dy dx))
;;         10000
;;         '(103 101))
