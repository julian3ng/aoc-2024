(defpackage :day-13
  (:use :cl :april :utils)
  (:export :part-1 :part-2))

(in-package :day-13)



(defun part-1 (input)
  (loop for presses in  (loop for (x y z) on input
                              when (and x y z)
                                collect
                                (let* ((xs (make-array '(2 2) :initial-contents (list x y)))
                                       (ys (make-array '(1 2) :initial-contents (list z)))
                                       (soln (april/let ((x xs)
                                                         (y ys))
                                               "(⌹⍉x)+.×⍉y"))
                                       (a-presses (aref soln 0 0))
                                       (b-presses (aref soln 1 0)))
                                  (when (and (zerop (rem a-presses 1)) (zerop (rem b-presses 1)))
                                    (+ (* a-presses 3) (* b-presses 1)))))
        when presses
          sum presses))


(part-1 (aoc/read-ints-ignoring-rest 13 :example)) ; => 480 (9 bits, #x1E0)
(part-1 (aoc/read-ints-ignoring-rest 13 :real)) ; => 28262 (15 bits, #x6E66)

(defun part-2 (input)
  (loop for presses in  (loop for (x y z) on input
                              when (and x y z)
                                collect
                                (let* ((xs (make-array '(2 2) :initial-contents (list x y)))
                                       (ys (make-array '(1 2) :initial-contents (list z)))
                                       (soln (april/let ((x xs)
                                                         (y ys))
                                               "(⌹⍉x)+.×⍉(y+10000000000000)"))
                                       (a-presses (aref soln 0 0))
                                       (b-presses (aref soln 1 0)))
                                  (when (and (zerop (rem a-presses 1)) (zerop (rem b-presses 1)))
                                    (+ (* a-presses 3) (* b-presses 1)))))
        when presses
          sum presses))

(part-2 (aoc/read-ints-ignoring-rest 13 :example)) ; => 875318608908 (40 bits, #xCBCD0E040C)
(part-2 (aoc/read-ints-ignoring-rest 13 :real)) ; => 101406661266314 (47 bits, #x5C3A9405CB8A)
