(defpackage :day-11
  (:use :cl :ppcre :april :utils)
  (:export :part-1 :part-2))

(in-package :day-11)

(defun count-digits (n)
  (if (zerop n) 1
      (1+ (floor (log n 10)))))

(defun next-gen (x)
  (let ((digits (count-digits x)))
    (cond ((zerop x) (list 1))
          ((evenp digits) (multiple-value-list (floor x (expt 10 (/ digits 2)))))
          (t (list (* 2024 x))))))

(defun part-1 (input blinks)
  (let ((generation (copy-seq input)))
    (loop repeat blinks do
      (setf generation (loop for x in generation nconc (next-gen x))))
    (length generation)))

;; (part-1 (aoc/read-int-list 11 :example) 25) ; => 55312 (16 bits, #xD810)
;; (part-1 (aoc/read-int-list 11 :real) 25) ; => 190865 (18 bits, #x2E991)

(defun bleh-memo (number blinks memo)
  (labels ((bleh (x b)
             (let ((result
                     (gethash (list x b)
                              memo)))
               (when (null result)
                 (setf (gethash (list x b) memo)
                       (cond ((zerop b) 1)
                             (t (apply #'+ (mapcar #'(lambda (xx) (bleh xx (1- b))) (next-gen x)))))))
               (gethash (list x b) memo))))
    (values (bleh number blinks))))



(defun part-2 (input)
  (let ((memo (make-hash-table :test 'equal)))
    (loop for x in input summing (bleh-memo x 75 memo))))


;; (part-2 (aoc/read-int-list 11 :example)) ; => 65601038650482 (46 bits, #x3BA9EE9B0C72)
;; (part-2 (aoc/read-int-list 11 :real)) ; => 225404711855335 (48 bits, #xCD011FC7E4E7)



