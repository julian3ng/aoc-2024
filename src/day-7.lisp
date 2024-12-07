(defpackage :day-7
  (:use :cl :april :ppcre :utils)
  (:export :part-1 :part-2))

(in-package :day-7)

(defun all-bitstrings (i)
  (loop for j from 0 to (1- (expt 2 i)) collecting (format nil "~v,'0b" i j)))

(defun part-1 (input)
  (apply #'+ (remove-if
              #'null
              (loop for line in input collect
                                      (let* ((target (car line))
                                             (others (cdr line))
                                             (numslots (1- (length others)))
                                             (mult-placements (all-bitstrings numslots)))
                                        (loop for mult-placement in mult-placements
                                              when
                                              (let ((cur (first others)))
                                                ;;(format t "placement: ~A target: ~A others: ~A~%" mult-placement target others)
                                                (loop for op across mult-placement
                                                      with j = 1
                                                      do
                                                         (if (char= op #\0)
                                                             (setf cur (+ cur (elt others j)))
                                                             (setf cur (* cur (elt others j))))
                                                         (incf j))
                                                (= target cur))
                                              return target))))))
;; (part-1 (aoc/read-ints-ignoring-rest 7 :example)) ; => 3749 (12 bits, #xEA5)
;; (part-1 (aoc/read-ints-ignoring-rest 7 :real)) ; => 3245122495150 (42 bits, #x2F3905FA2AE)

(defun all-tritstrings (i)
  (loop for j from 0 to (1- (expt 3 i)) collecting (format nil "~3,v,'0r" i j)))

(defun part-2 (input)
  (apply #'+ (remove-if
              #'null
              (loop for line in input collect
                                      (let* ((target (car line))
                                             (others (cdr line))
                                             (numslots (1- (length others)))
                                             (op-placements (all-tritstrings numslots)))
                                        (loop for op-placement in op-placements
                                              when
                                              (let ((cur (first others)))
                                                ;;(format t "placement: ~A target: ~A others: ~A~%" op-placement target others)                                                
                                                (loop for op across op-placement
                                                      with j = 1
                                                      do
                                                         (cond
                                                           ((char= op #\0) (setf cur (+ cur (elt others j))))
                                                           ((char= op #\1) (setf cur (* cur (elt others j))))
                                                           ((char= op #\2) (setf cur
                                                                                 (parse-integer
                                                                                  (concatenate 'string
                                                                                               (write-to-string cur)
                                                                                               (write-to-string (elt others j)))))))
                                                         (incf j))
                                                (= target cur))
                                              return target))))))

;; (part-2 (aoc/read-ints-ignoring-rest 7 :example)) ; => 11387 (14 bits, #x2C7B)
;; (part-2 (aoc/read-ints-ignoring-rest 7 :real)) ; => 105517128211543 (47 bits, #x5FF79EF1DC57)105517128211543 (47 bits, #x5FF79EF1DC57)
