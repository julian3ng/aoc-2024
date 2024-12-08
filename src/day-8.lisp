(defpackage :day-8
  (:use :cl :april :ppcre :utils :alexandria)
  (:export :part-1 :part-2))

(in-package :day-8)

(defun make-antinodes (p1 p2)
  (destructuring-bind (y1 x1) p1
    (destructuring-bind (y2 x2) p2
      (let ((dy (- y2 y1))
            (dx (- x2 x1)))
        `((,(- y1 dy) ,(- x1 dx))
          (,(+ y2 dy) ,(+ x2 dx)))))))

(defun in-map (p max-y max-x)
  (destructuring-bind (y x) p
    (and
     (<= 0 y (1- max-y))
     (<= 0 x (1- max-x)))))

(defun find-nodes (board char)
  (april (with (:state :in ((x board)
                            (c char))))
         "(⍸ c = x) - 1"))

(defun count-antinodes (board)
  (april/x board "+/∊'#' = x"))

(defun part-1 (input)
  (let ((antinodes (make-hash-table :test 'equal)))
    (destructuring-bind (max-y max-x) (array-dimensions input)
      (loop for char across "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" do 
        (let ((points (find-nodes input char)))
          (loop for i from 1 to (1- (length points)) do
            (let ((p1 (coerce (aref points i) 'list)))
              (loop for j from 0 to (1- i)
                    do
                       (let ((p2 (coerce (aref points j) 'list)))
                         (loop for anti in (make-antinodes p1 p2)
                               when (in-map anti max-y max-x)
                                 do (setf (gethash anti antinodes) t)))))))))
    (loop for h being the hash-keys of antinodes count h)))

;; (part-1 (aoc/read-matrix 8 :example)) ; => 14 (4 bits, #xE, #o16, #b1110)
;; (part-1 (aoc/read-matrix 8 :real)) ; => 379 (9 bits, #x17B)


(defun make-antinodes-2 (p1 p2 max-y max-x)
  (destructuring-bind (y1 x1) p1
    (destructuring-bind (y2 x2) p2
      (let ((dy (- y2 y1))
            (dx (- x2 x1)))
        `(
          ,@(loop for mult from 0 until (not (in-map `(,(- y1 (* mult dy))
                                                       ,(- x1 (* mult dx)))
                                                     max-y max-x))
                  collect `(,(- y1 (* mult dy)) ,(- x1 (* mult dx))))
          ,@(loop for mult from 0 until (not (in-map `(,(+ y2 (* mult dy))
                                                       ,(+ x2 (* mult dx)))
                                                     max-y max-x))
                  collect `(,(+ y2 (* mult dy)) ,(+ x2 (* mult dx)))))
        ))))

(defun part-2 (input)
  (let ((antinodes (make-hash-table :test 'equal)))
    (destructuring-bind (max-y max-x) (array-dimensions input)
      (loop for char across "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" do 
        (let ((points (find-nodes input char)))
          (loop for i from 1 to (1- (length points)) do
            (let ((p1 (coerce (aref points i) 'list)))
              (loop for j from 0 to (1- i)
                    do
                       (let ((p2 (coerce (aref points j) 'list)))
                         (loop for anti in (make-antinodes-2 p1 p2 max-y max-x)
                               when (in-map anti max-y max-x)
                                 do (setf (gethash anti antinodes) t)))))))))
    (loop for k being the hash-keys of antinodes count k)))


;; (part-2 (aoc/read-matrix 8 :example)) => 34 (6 bits, #x22, #o42, #b100010)
;; (part-2 (aoc/read-matrix 8 :real)) => 1339 (11 bits, #x53B)
