(defpackage :day-1
  (:use :cl)
  (:export :part-1 :part-2))

(in-package :day-1)

(defun part-1 (input)
  (let ((list1  (loop for line in input collect
                                        (let* ((parts (uiop:split-string line))
                                               (a (parse-integer (first parts))))
                                          a)))
        (list2  (loop for line in input collect
                                        (let* ((parts (uiop:split-string line))
                                               (a (parse-integer (second parts))))
                                          a))))
    (let ((sorted-list1 (sort list1 #'<))
          (sorted-list2 (sort list2 #'<)))
      (loop for i from 0 to (1- (length sorted-list1))
            summing (abs  (-  (nth i sorted-list2) (nth i sorted-list1)))))))

(defun part-2 (input)
  (let ((list1  (loop for line in input collect
                                        (let* ((parts (uiop:split-string line))
                                               (a (parse-integer (first parts))))
                                          a)))
        (list2  (loop for line in input collect
                                        (let* ((parts (uiop:split-string line))
                                               (a (parse-integer (second parts))))
                                          a))))
    (loop for i from 0 to (1- (length list1)) summing
                                              (let ((appearances (count (nth i list1) list2)))
                                                (*  (nth i list1) appearances)))))
