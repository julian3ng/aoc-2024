(defpackage :day-10
  (:use :cl :april :ppcre :utils)
  (:export :part-1 :part-2))

(in-package :day-10)

(defun neighbors-4 (p bounds)
  (destructuring-bind (my mx) bounds
    (destructuring-bind (y x) p
      (labels ((in-bounds (y x)
                 (and (<= 0 y (1- my))
                      (<= 0 x (1- mx)))))
        (loop for (i j) in '((-1 0) (0 1) (1 0) (0 -1))
              when (in-bounds (+ y i) (+ x j))
                collect (list (+ y i) (+ x j)))))))

(defun find-nines (board)
  (let ((nines (make-hash-table :test 'equalp))
        (visited (make-hash-table :test 'equalp))
        (bounds (array-dimensions board)))
    (labels ((visit (p) (setf (gethash p visited) t))
             (visited (p) (gethash p visited))
             (add-nine (p) (if (gethash p nines)
                               (incf (gethash p nines))
                               (setf (gethash p nines) 1)))
             (dfs (p start)
               (visit p)
               (destructuring-bind (y x) p
                 (let ((board-value (aref board y x)))
                   ;;(format t "(~A ~A) = ~A~%" y x board-value)
                   (when (= board-value 9)
                     (add-nine start)
                     (return-from dfs))
                   (loop for neighbor in (neighbors-4 p bounds)
                         when (and
                               (not (visited neighbor))
                               (= (aref board (first neighbor) (second neighbor)) (1+ board-value)))
                           do
                              (dfs neighbor start))))))
      (loop for p across (april/x board "1-⍨⍸x=0") do
        (let ((start (coerce p 'list)))
          (dfs start start)
          (setf visited (make-hash-table :test 'equalp))))
      nines)))

(defun part-1 (input)
  (let ((nines (find-nines input)))
    (loop for v being the hash-values of nines sum v)))

(defun find-nines-2 (board)
  (let ((nines (make-hash-table :test 'equalp))
        (bounds (array-dimensions board)))
    (labels ((add-nine (p) (if (gethash p nines)
                               (incf (gethash p nines))
                               (setf (gethash p nines) 1)))
             (dfs (p start)
               (destructuring-bind (y x) p
                 (let ((board-value (aref board y x)))
                   (when (= board-value 9)
                     (add-nine start)
                     (return-from dfs))
                   (loop for neighbor in (neighbors-4 p bounds)
                         when (= (aref board (first neighbor) (second neighbor)) (1+ board-value))
                           do
                              (dfs neighbor start))))))
      (loop for p across (april/x board "1-⍨⍸x=0") do
        (let ((start (coerce p 'list)))
          (dfs start start)))
      nines)))

;; (part-1 (aoc/read-int-matrix 10 :example)) ; => 36 (6 bits, #x24, #o44, #b100100)
;; (part-1 (aoc/read-int-matrix 10 :real)) ; => 496 (9 bits, #x1F0)

(defun part-2 (input) input
  (let ((nines (find-nines-2 input)))
    (loop for v being the hash-values of nines sum v)))

;; (part-2 (aoc/read-int-matrix 10 :example)) ; => 81 (7 bits, #x51, #o121, #b1010001)
;; (part-2 (aoc/read-int-matrix 10 :real))  ; => 1120 (11 bits, #x460)
