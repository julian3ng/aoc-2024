(defpackage :day-9
            (:use :cl :utils :april)
            (:export :part-1 :part-2))

(in-package :day-9)
(proclaim '(optimize (debug 3) (speed 0)))

(defun spec-size (input)
  (loop for c across input sum (digit-char-p c)))

(defun gen-spec (input)
  (let* ((size (spec-size input))
         (spec (make-array size :initial-element -1)))
    (loop for c across input
          and i from 0
          with spec-ix = 0
          do
             (let ((id (floor i 2))
                   (slot-size (digit-char-p c)))
               (when (= 0 (mod i 2))
                   (dotimes (ii slot-size) (setf (aref spec (+ spec-ix ii)) id)))
               (incf spec-ix slot-size)))
    spec))



(defun compact (spec)
  (loop with i = 0
        with j = (1- (length spec))
        until (> i j)
        do
           (loop until (< (aref spec i) 0) do (incf i))
           (loop until (>= (aref spec j) 0) do (decf j))
           (rotatef (aref spec i) (aref spec j))
        finally (rotatef (aref spec i) (aref spec j))
)

  spec)

;; (defun rising-edges (spec)
;;   (let ((edges (list (list 0 (aref spec 0)))))
;;     (loop for x across spec
;;           and i from 0
;;           with cur = (aref spec 0)
;;           do
;;              (when (/= x cur)
;;                (push (list i x) edges)
;;                (setf cur x)))
;;     edges))

(defun score (spec)
  (loop for id across spec and pos from 0 when (>= id 0) sum (* id pos)))

(score (compact (gen-spec (aoc/read-input 9 :example))))

(defun part-1 (input)
  (score (compact (gen-spec input))))

(part-1 (utils:aoc/read-input 9 :example)) ; => 1928 (11 bits, #x788)
(part-1 (utils:aoc/read-input 9 :real)) ; => 6288599492129 (43 bits, #x5B82DBD2621)

(defparameter *emptys* (make-array 9 :initial-element -1))
(defun compact-2 (spec)
  (let ((id (aref spec (1- (length spec))))
        (i nil)
        (j (length spec)))
    (loop until (< id 0) do
      (loop until (or (<= j 0) (= (aref spec (1- j)) id)) do (decf j))
      (setf i j)
      (loop until (or (<= i 0) (/= (aref spec (1- i)) id)) do (decf i))
      ;;      (format t "~A ~A~%" i j)
      (let ((size (- j i)))
        (loop for k from 0 to (min (- (1- (length spec)) size) i) do
          ;;        (format t "k: ~A ~A~%" k (+ k size))
          (when (every (lambda (x) (= -1 x)) (subseq spec k (+ k size)))
            (rotatef (subseq spec k (+ k size)) (subseq spec i j))
            (return))))
      (decf id))
    spec))




(defun part-2 (input)
  (score (compact-2 (gen-spec input))))

;; (part-2 (aoc/read-input 9 :example)) ; => 2858 (12 bits, #xB2A)
;; (part-2 (aoc/read-input 9 :real)) ; => 6321896265143 (43 bits, #x5BFEE6195B7)


