(defpackage :day-16
  (:use :cl :april :utils)
  (:export :part-1 :part-2))

(in-package :day-16)

(defclass queue ()
  ((in-stack :initform nil :accessor in-stack)
   (out-stack :initform nil :accessor out-stack)))

(defmethod emptyp ((q queue))
  (with-slots (in-stack out-stack) q
    (eq t (and (null in-stack) (null out-stack)))))

(defmethod enqueue ((q queue) x)
  (with-slots (in-stack) q
    (push x in-stack)))

(defmethod dequeue ((q queue) )
  (with-slots (in-stack out-stack) q
    (cond
      ((emptyp q) nil)
      ((null out-stack) (progn
                          (setq out-stack (nreverse in-stack))
                          (setq in-stack nil)
                          (pop out-stack)))
      (t (pop out-stack)))))

(defmethod print-object ((object queue) stream)
  (with-slots (in-stack out-stack) object
    (format t "[~{~A ~}] [~{~A ~}]" in-stack out-stack)))

(defun bfs (board)
  (declare (optimize (debug 3)))
  (let* ((bounds (array-dimensions board))
         (start (cons 'r (coerce (april/x board "⊃1-⍨⍸x='S'") 'list)))
         (end (coerce (april/x board "⊃1-⍨⍸x='E'") 'list))
         (dist (make-hash-table :test 'equal))
         (prev (make-hash-table :test 'equal))
         (q (make-instance 'queue))
         (i 0)
         (max-iter 100000000))
    (labels ((is-space (x) (char= (aref board (second x) (third x)) #\.))
             (is-end (x) (char= (aref board (second x) (third x)) #\E))
             (score-turn (h1 h2)
               (cond
                 ((eq h1 h2) 1)
                 ((or (and (eq h1 'u) (eq h2 'd))
                      (and (eq h1 'd) (eq h2 'u))
                      (and (eq h1 'l) (eq h2 'r))
                      (and (eq h1 'r) (eq h2 'l))) 2001)
                 (t 1001))))
      (enqueue q start)
      (setf (gethash start dist) 0)
      (setf (gethash start prev) nil)
      (loop until (or (emptyp q) (<= max-iter i)) do
        (incf i)
        ;; get current coords and heading for consideration
        (let ((current (dequeue q)))
          (when current
            (destructuring-bind (ch cy cx) current
              (let ((cd (gethash current dist)))
                (loop for dy in '(-1 0 1 0)
                      and dx in '(0 1 0 -1)
                      and h  in '(u r d l)
                      do
                         (let ((neighbor (list h (+ dy cy) (+ dx cx))))
                           (when (or (is-space neighbor) (is-end neighbor))
                             (let ((cur-neighbor-dist (gethash neighbor dist))
                                   (new-neighbor-dist (+ cd (score-turn ch h))))
                               (when (or (null cur-neighbor-dist)
                                         (<= new-neighbor-dist cur-neighbor-dist))
                                 (cond
                                   ((null cur-neighbor-dist) (setf (gethash neighbor prev) (list current)))
                                   ((= new-neighbor-dist cur-neighbor-dist) (push current (gethash neighbor prev)))
                                   ((< new-neighbor-dist cur-neighbor-dist) (setf (gethash neighbor prev) (list current))))
                                 (setf (gethash neighbor dist) new-neighbor-dist)
                                 (enqueue q neighbor))))))))))))
    ;; (loop for y from 0 to (1- (first bounds)) do
    ;;   (format t "~&~3A: " y)
    ;;   (loop for x from 0 to (1- (second bounds)) do
    ;;     (format t "~5A " (loop for h in '(u r d l)
    ;;                             for v = (gethash (list h y x) dist)
    ;;                             when v
    ;;                             minimizing v))))

    (values dist prev end)))



(defun part-1 (input-matrix)
  (multiple-value-bind (dist prev end) (bfs input-matrix)
    (loop for h in '(u r d l)
          for v = (gethash (cons h end) dist)
          when v
            minimize v)))

;; (part-1 (aoc/read-matrix 16 :example)) ; => 7036 (13 bits, #x1B7C)
;; (part-1 (aoc/read-matrix 16 :real)) ; => 99460 (17 bits, #x18484)

(defun part-2 (input-matrix)
  (multiple-value-bind (dist prev end) (bfs input-matrix)
    (let ((min-dist (loop for h in '(r d l u)
                          for v = (gethash (cons h end) dist)
                          when v
                            minimize v)))
      (let ((best-ends (loop for h in '(u r d l)
                             for v = (gethash (cons h end) dist)
                             when (and (not (null v)) (= min-dist v))
                               collect (cons h end)))
            (min-path-nodes (make-hash-table :test 'equal))
            (min-path-positions (make-hash-table :test 'equal)))
        ;; backtrack
        (labels ((backtrack (p)
                   ;;        (format t "~A <- ~{~A ~}~%" p (gethash p prev))
                   (setf (gethash p min-path-nodes) t)
                   (setf (gethash (subseq p 1) min-path-positions) t)
                   (loop for pred in (gethash p prev)
                         unless (gethash pred min-path-nodes)
                           do (backtrack pred))))
          (loop for e in best-ends do (backtrack e)))
        (loop for k being the hash-keys of min-path-nodes do
          (setf (aref input-matrix (second k) (third k)) #\O))
        ;; (format t "~&")
        ;; (loop for y from 0 to (1- (first (array-dimensions input-matrix))) do
        ;;   (loop for x from 0 to (1- (second (array-dimensions input-matrix))) do
        ;;     (format t "~A" (aref input-matrix y x)))
        ;;   (format t "~%"))

        (loop for k being the hash-keys of min-path-positions count k)))))


(part-2 (aoc/read-matrix 16 :example)) ; => 45 (6 bits, #x2D, #o55, #b101101)
(part-2 (aoc/read-matrix 16 :real)) ; => 500 (9 bits, #x1F4)
