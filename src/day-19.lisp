(defpackage :day-19
  (:use :cl :alexandria :ppcre :utils)
  (:export :part-1 :part-2))

(in-package :day-19)

(defun can-partition (dictionary word)
  ;; Base case
  ;; empty string is trivially partitionable
  ;; s[0..i] is partitionable if s[0..j] is partitionable and s[j..i] is a word in the dictionary.
  
  (let ((table (make-array (1+ (length word)) :element-type 'bit)))
    (setf (aref table 0) 1)
    (loop for i from 1 to (length word)
          do
             (loop for j from 0 to (1- i) do
               (when (and (= 1 (aref table j)) (find (subseq word j i) dictionary :test 'equal))
                   (setf (aref table i) 1))))
    (aref table (length word))))

(defun parse-input (input)
  (values
   (remove-if (lambda (s) (zerop (length s))) (uiop:split-string (first input) :separator '(#\  #\,)))
   (cddr input)))

(parse-input (aoc/read-input-list 19 :example))


(defun part-1 (input)
  (multiple-value-bind (dictionary words) (parse-input input)
    (loop for word in words counting (= 1 (can-partition dictionary word)))))

(part-1 (utils:aoc/read-input-list 19 :example)) ; => 6 (3 bits, #x6, #o6, #b110)
(part-1 (utils:aoc/read-input-list 19 :real)) ; => 336 (9 bits, #x150)


(defun part-2 (input)
  (declare (optimize (debug 3)))
  (multiple-value-bind (dictionary words) (parse-input input)
    (let ((cache (make-hash-table :test 'equal)))
      (labels ((dp (word)
                 (let ((maybe (gethash word cache)))
                   (when (null maybe)
                     (setf (gethash word cache)
                           (if (equal word "")
                               1
                               (loop for dword in dictionary
                                     sum
                                     (let ((pos (search dword word)))
                                       (cond
                                         ((null pos) 0)
                                         ((zerop pos) (dp (subseq word (length dword))))
                                         (t 0))
                                       )))))
                   (gethash word cache))))
        (loop for word in words sum (dp word))))))

;; (part-2 (aoc/read-input-list 19 :example)) ; => 16 (5 bits, #x10, #o20, #b10000)
;; (part-2 (aoc/read-input-list 19 :real)) ; => 758890600222015 (50 bits, #x2B234FCA64D3F)
