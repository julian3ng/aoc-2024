(defpackage :day-18
  (:use :cl :april :utils :my-queue)
  (:export :part-1 :part-2))

(in-package :day-18)

(defun part-1 (input steps &key mode)
  (declare (optimize (debug 3)))
  (let* ((size (case mode
                 (example 7)
                 (real 71)))
         (board (make-array (list size size) :initial-element -1)))
    ;; Drop bytes
    (loop for (x y) in input
          and i from 0 to steps
          do (setf (aref board y x) i))
    (format t "================================================================~%")
    (loop for y from 0 to (1- size) do
      (loop for x from 0 to (1- size) do
        (format t "~:[.~;#~]" (>= (aref board y x) 0)))
      (format t "~%"))
    (let ((q (make-instance 'my-queue:queue))
          (dists (make-array (list size size) :initial-element -1)))
      (labels ((in-bounds (p) (and (<= 0 (first p) (1- size))
                                   (<= 0 (second p) (1- size))))
               (is-free (p) (< (apply #'aref board p) 0))
               )
        (my-queue:enqueue q (list 0 0) )
        (setf (aref dists 0 0) 0)
        (loop until (my-queue:emptyp q) do
          (let ((current (my-queue:dequeue q)))
            (destructuring-bind (y x) current
              (loop for dy in '(-1 0 1 0)
                    and dx in '(0 1 0 -1)
                    do
                       (let ((neighbor (list (+ y dy) (+ x dx))))
                         (when (and (in-bounds neighbor) (is-free neighbor))
                           (let ((cur-dist (apply #'aref dists neighbor))
                                 (new-dist (1+ (apply #'aref dists current))))
                             (when (or (< cur-dist 0)
                                       (< new-dist cur-dist))
                               (setf (apply #'aref dists neighbor) new-dist)
                               (my-queue:enqueue q neighbor))))))))))
      (loop for y from 0 to (1- size) do
        (loop for x from 0 to (1- size) do
          (format t "~4A " (aref dists y x)))
        (format t "~%"))      
      
      (aref dists (1- size) (1- size)))))

;; (part-1 (utils:aoc/read-ints-ignoring-rest 18 :example) 12 :mode 'example)
;; (part-1 (utils:aoc/read-ints-ignoring-rest 18 :real) 1024 :mode 'real)

(defun part-2 (input &key mode)
  (let* ((r  (length input))
         (m (floor r 2))
         (l 0))
    (loop until (or (<= m l) (>= m r)) do
      (let ((value (part-1 input m :mode mode)))
        (format t "~A ~A ~A: ~A~%" l m r value)
        (if (< value 0)
            (setf r m m (+ l (floor (- r l) 2)))
            (setf l m m (+ l (floor (- r l) 2))))))
    (format t "~A ~A ~A~%" l m r)))

;; Binary search then do it by inspection...
;; (part-2 (utils:aoc/read-ints-ignoring-rest 18 :example) :mode 'example)
;; (part-2 (utils:aoc/read-ints-ignoring-rest 18 :real) :mode 'real)
;; (elt (aoc/read-ints-ignoring-rest 18 :real) 2935)
