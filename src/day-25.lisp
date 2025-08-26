(defpackage :day-25
  (:use :cl :april :utils))

(in-package :day-25)



(defun get-locks-and-keys (input)
  (let ((locks-and-keys (loop for matrix in (ppcre:split "\\n\\n" input)
                              collect (april/x
                                          (make-array (list 7 5) :initial-contents (uiop:split-string matrix :separator '(#\newline))) "'#'=x"))))
    (values
     (loop for thing in locks-and-keys when (loop for i from 0 to 4 always (= (aref thing 0 i) 1)) collect thing)
     (loop for thing in locks-and-keys when (loop for i from 0 to 4 always (= (aref thing 0 i) 0)) collect thing))))

(defun smoosh-matrix (matrix)
  (april/x matrix "1-⍨+⌿x"))



(defun part-1 (input)
  (multiple-value-bind (locks keys) (get-locks-and-keys input)
    (let ((smooshed-locks (mapcar #'smoosh-matrix locks))
          (smooshed-keys (mapcar #'smoosh-matrix keys)))
      (loop for lock in smooshed-locks sum
            (loop for key in smooshed-keys
                  sum
                  (let ((v (april/let ((x lock)
                                      (y key))
                            "5=+/5≥(x+y)")))
                    (if (= v 1)
                        1
                        0)))))))

;; (part-1 (aoc/read-input 25 :real)) ==> 3201 (12 bits, #xC81)
