(defpackage :utils
  (:use :cl :april)
  (:export
   :aoc/read-input
   :aoc/read-input-list
   :aoc/read-input-array
   :aoc/read-int-array-of-arrays
   :aoc/read-int-list-of-lists
   :aoc/read-int-array))

(in-package :utils)

(defun get-input-path (day mode)
  (let*
      ((filename (cond ((eq mode :example) "example.txt")
                       ((eq mode :real) "real.txt")))
       (input-path (format nil "~~/common-lisp/aoc-2024/tests/input/day-~A/~A" day filename)))
    input-path))

(defun aoc/read-input (day mode)
  (uiop:read-file-string (get-input-path day mode)))

(defun aoc/read-input-list (day mode)
  (uiop:read-file-lines (get-input-path day mode)))

(defun aoc/split-and-read-ints (lines)
  (mapcar
   (lambda (l)
     (mapcar #'parse-integer (uiop:split-string l)))
   lines))

(defun aoc/read-int-list-of-lists (day mode)
  (aoc/split-and-read-ints (aoc/read-input-list day mode)))

(defun aoc/read-input-array (day mode)
  (let* ((lines (aoc/read-input-list day mode))
         (arr (make-array
               (length lines)
               :initial-contents lines)))
    arr))

(defun aoc/read-int-array-of-arrays (day mode)
  (let ((ints (aoc/split-and-read-ints
               (aoc/read-input-list day mode))))
    (make-array (length ints)
                :initial-contents (mapcar
                                   (lambda (row) (make-array (length row) :initial-contents row))
                                   ints))))

(defun aoc/read-int-array (day mode)
  (let ((ints (aoc/split-and-read-ints (aoc/read-input-list day mode))))
    (make-array (list (length ints) (length (first ints)))
                :initial-contents ints)
    ))
