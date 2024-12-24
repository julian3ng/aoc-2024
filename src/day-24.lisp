(defpackage :day-24
  (:use :cl :utils :ppcre))

(in-package :day-24)

(defun parse-input (lines)
  (multiple-value-bind (starts i)
      (loop for line in lines
            for i from 0
            while (string/= line "")
            collect (ppcre:split ":" line) into starts
            finally (return (values starts i)))
    (values starts (loop for line in (subseq lines (1+ i))
                         collect (uiop:split-string line :separator " ")))))


(defun build-graph (starts graph-lines)
  (let ((graph (make-hash-table :test 'equal)))
    (loop for (x value) in starts do
      (setf (gethash x graph) (parse-integer value :junk-allowed t)))
    (loop for (x op y _ z) in graph-lines do
      (setf (gethash z graph) (list op x y)))
    graph))

(defun part-1 (lines)
  (multiple-value-bind (starts graph-lines) (parse-input lines)
    (let ((graph (build-graph starts graph-lines)))
      (labels ((dfs (node)
                 (let ((desc (gethash node graph)))
                   (if (numberp desc)
                       desc
                       (destructuring-bind (op x y) desc
                         (cond
                           ((string= "AND" op) (logand (dfs x) (dfs y)))
                           ((string= "OR" op) (logior (dfs x) (dfs y)))
                           ((string= "XOR" op) (logxor (dfs x) (dfs y)))
                           ))))))

        (let ((bits (mapcar #'dfs (sort (loop for l in graph-lines
                                              when (char= (aref (fifth l) 0) #\z)
                                                collect (fifth l))
                                        #'string>))))
          (loop with x = 0
                for bit in bits
                do
                   (case bit
                     (1 (setf x (1+ (ash x 1))))
                     (0 (setf x (ash x 1))))
                finally (return x)))))))

(part-1 (aoc/read-input-list 24 :small)) ; => 4 (3 bits, #x4, #o4, #b100)
(part-1 (aoc/read-input-list 24 :medium)) ; => 2024 (11 bits, #x7E8)
(part-1 (aoc/read-input-list 24 :real)) ; => 47666458872582 (46 bits, #x2B5A36594B06)

(defun get-inputs (graph)
  (let ((xbits (mapcar (lambda (x) (gethash x graph))
                       (sort (loop for k being the hash-keys of graph
                                   when (char= (aref k 0) #\x)
                                     collect k)
                             #'string>)))
        (ybits (mapcar (lambda (y) (gethash y graph))
                       (sort (loop for k being the hash-keys of graph
                                   when (char= (aref k 0) #\y)
                                     collect k)
                             #'string>))))
    (loop with (x y) = (list 0 0)
          for xb in xbits
          and yb in ybits
          do
             (case xb
               (1 (setf x (1+ (ash x 1))))
               (0 (setf x (ash x 1))))
             (case yb
               (1 (setf y (1+ (ash y 1))))
               (0 (setf y (ash y 1))))
          finally (return (values x y)))))

(defun calculate-value (node graph &optional (depth 0))
  (when (= depth 0)
    (format t "================================================================~%"))  
  (format t "~A (depth ~A) = " node depth)
  (let ((desc (gethash node graph)))
    (if (numberp desc)
        (progn
          (format t "~A~%" desc)
          desc)
        (destructuring-bind (op x y) desc
          (format t "~A ~A ~A~%" op x y)
          (let ((vx (calculate-value x graph (1+ depth)))
                (vy (calculate-value y graph (1+ depth))))
            (cond
              ((string= "AND" op) (logand vx vy))
              ((string= "OR" op) (logior vx vy))
              ((string= "XOR" op) (logxor vx vy))))))))

(defun get-output (graph)
  (let ((zbits (mapcar (lambda (z) (calculate-value z graph))
                       (sort (loop for k being the hash-keys of graph
                                   when (char= (aref k 0) #\z)
                                     collect k)
                             #'string>))))
    (loop with z = 0
          for zb in zbits
          do (case zb
               (1 (setf z (1+ (ash z 1))))
               (0 (setf z (ash z 1))))
          finally (return z))))



(defun part-2 (lines)
  (multiple-value-bind (starts graph-lines) (parse-input lines)
    (let ((graph (build-graph starts graph-lines)))
      (get-output graph))))

(part-2 (aoc/read-input-list 24 :real))

(multiple-value-bind (x y) (get-inputs
                            (apply #'build-graph (multiple-value-list (parse-input (aoc/read-input-list 24 :real)))))
  (format t " ~,,,4:B~% ~,,,4:B~%" x y)
  (format t "~,,,4:B~%" (part-1 (aoc/read-input-list 24 :real))))


z45 should be

