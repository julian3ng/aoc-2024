(defpackage :day-22
            (:use :cl :utils))

(in-package :day-22)

(defun evolve (number)
  (let* ((a (mod (logxor (* number 64) number) 16777216))
         (b (mod (logxor (floor a 32) a) 16777216))
         (c (mod (logxor (* b 2048) b) 16777216)))
    c))

(let ((x 1))
  (loop repeat 2000 do (setf x (evolve x)))
  (print x))

(defun part-1 (input)
  (let ((nums (apply #'nconc input)))
    (loop for num in nums summing
          (let ((x num))
            (loop repeat 2000 do (setf x (evolve x)))
            x))))

;; (part-1 (aoc/read-int-list-of-lists 22 :real)) ; => 14392541715 (34 bits, #x359DCC213)

(defun get-prices (number)
  (let ((x number))
    (loop repeat 2000
          collect
          (prog1 (nth-value 1 (floor x 10))
                 (setf x (evolve x))))))

(defun get-diffs (prices)
  (loop for (x y) on prices when y collect (- y x)))

(defun part-2 (input)
  (let ((nums (apply #'nconc input))
        (sums (make-hash-table :test 'equal))
        (cur-sequences (make-hash-table :test 'equal)))
    ;; generate last digits and diffs of last digits
    (let* ((num-prices (loop for num in nums collect (get-prices num)))
           (price-changes (loop for prices in num-prices
                                collect (get-diffs prices))))
      ;; loop in parallel
      (loop
        for prices in num-prices
        and changes in price-changes
        do
           ;; first prices doesn't have a diff associated with it
           (loop
             for (pa pb pc pd) on (subseq prices 1) 
             and (ca cb cc cd) on changes
             when (and pd cd)
               do
                  (let ((seq (list ca cb cc cd)))
                    ;; skip sequences we've already seen, since they'll never be used
                    (unless (gethash seq cur-sequences)
                      (setf (gethash seq cur-sequences) t)
                      (if (gethash seq sums)
                          (incf (gethash seq sums) pd)
                          (setf (gethash seq sums) pd)))))
           ;; reset
           (setf cur-sequences (make-hash-table :test 'equal))))
    (loop for v being the hash-values of sums maximize v)))

;; (part-2 '((1) (2) (3) (2024))) ; => 23 (5 bits, #x17, #o27, #b10111)
;; (part-2 (aoc/read-int-list-of-lists 22 :example)) ; => 24 (5 bits, #x18, #o30, #b11000)
;; (part-2 (aoc/read-int-list-of-lists 22 :real)) ; => 1628 (11 bits, #x65C)


