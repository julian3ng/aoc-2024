(defpackage :utils
  (:use :cl :april :ppcre)
  (:export
   :aoc/read-input
   :aoc/read-input-list
   :aoc/read-input-array
   :aoc/read-int-array-of-arrays
   :aoc/read-int-list-of-lists
   :aoc/read-int-array
   :aoc/read-matrix
   :aoc/read-int-matrix
   :april/x
   :april-f/x
   :aoc/read-ints-ignoring-rest
   :april/let
   :april-f/let
   :aoc/read-int-list
   ))

(in-package :utils)

(defun get-input-path (day mode)
  (let*
      ((filename (cond ((eq mode :example) "example.txt")
                       ((eq mode :real) "real.txt")))
       (input-path (format nil "~~/common-lisp/aoc-2024/tests/input/day-~A/~A" day filename)))
    input-path))

(defun aoc/read-input (day mode)
  (string-trim '(#\newline)  (uiop:read-file-string (get-input-path day mode))))

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
                :initial-contents ints)))

(defun aoc/read-matrix (day mode)
  (let ((input (aoc/read-input-list day mode)))
    (make-array (list (length input) (length (first input))) :initial-contents input)))

(defun aoc/read-int-matrix (day mode)
  (let ((input (aoc/read-input-list day mode)))
    (make-array (list (length input) (length (first input))) :initial-contents
                (loop for row in input collect
                                       (loop for cell across row collect (digit-char-p cell))))))

(defmacro april/x (input &body body)
  (let ((x (gensym)))
    `(let ((,x ,input))
       (april (with (:state :in ((x ,x))))
              ,@body))))

(defmacro april-f/x (input &body body)
  (let ((x (gensym)))
    `(let ((,x ,input))
       (april-f (with (:state :in ((x ,x))))
                ,@body))))

(defun aoc/read-ints-ignoring-rest (day mode)
  (let ((input (aoc/read-input-list day mode)))
    (loop for line in input
          collecting
          (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" line)))))

(defmacro april/let (bindings &body body)
  `(april (with (:state :in (,@bindings)))
          ,@body))

(defmacro april-f/let (bindings &body body)
  `(april-f (with (:state :in (,@bindings)))
            ,@body))

(defun aoc/read-int-list (day mode)
  (first (aoc/split-and-read-ints (aoc/read-input-list day mode))))
