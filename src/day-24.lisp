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

;; (part-1 (aoc/read-input-list 24 :small)) ; => 4 (3 bits, #x4, #o4, #b100)
;; (part-1 (aoc/read-input-list 24 :medium)) ; => 2024 (11 bits, #x7E8)
;; (part-1 (aoc/read-input-list 24 :real)) ; => 47666458872582 (46 bits, #x2B5A36594B06)

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
  ;; (when (= depth 0)
  ;;   (format t "================================================================~%"))  
  ;; (format t "~A (depth ~A) = " node depth)
  (let ((desc (gethash node graph)))
    (if (numberp desc)
        (progn
;;          (format t "~A~%" desc)
          desc)
        (destructuring-bind (op x y) desc
;;          (format t "~A ~A ~A~%" op x y)
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

(defun gate-type (gate) (and (not (null gate)) (listp gate) (first gate)))

(defun fix-graph (graph)
  (declare (optimize (debug 3)))
  (labels
      ((is-half-sum (node num)
         ;; if num-string = 01
         ;; either:
         ;;   - node is XOR x01 y01
         ;;   - node is XOR y01 x01
         (let ((gate (gethash node graph))
               (num-string (format nil "~2,'0D" num)))
           (and
            (equal "XOR" (gate-type gate))
            (ppcre:scan num-string (second gate))
            (ppcre:scan num-string (third gate)))))
       (is-half-carry (node num)
         (let ((gate (gethash node graph))
               (num-string (format nil "~2,'0D" num)))
           (and
            (equal "AND" (gate-type gate))
            (ppcre:scan num-string (second gate))
            (ppcre:scan num-string (third gate)))))
       (is-full-carry (node num)
         (if (= num 0)
             (is-half-carry node num)
             (let ((gate (gethash node graph)))
               (and
                (equal "OR" (gate-type gate))
                (let ((left-gate (gethash (second gate) graph))
                      (right-gate (gethash (third gate) graph)))
                  (and
                   (gate-type left-gate)
                   (gate-type right-gate)
                   (or
                    ;; First input is half sum of this level
                    ;; and full carry of last level in either order
                    ;; Second input is half carry of this level
                    (and
                     (is-half-carry (third gate) num)
                     (or
                      ;; First input is the AND of half sum for same level
                      ;; and full carry of previous level
                      ;; Second input is the half carry for the same level
                      (and
                       (is-half-sum (second left-gate) num)
                       (is-full-carry (third left-gate) (1- num))
                       )
                      (and
                       (is-full-carry (second left-gate) (1- num))                     
                       (is-half-sum (third left-gate) num))))
                    (and
                     (is-half-carry (second gate) num)
                     (or
                      ;; First input is the half carry for the same level
                      ;; Second input is the AND of half sum for same level
                      ;; and full carry of previous level
                      (and
                       (is-half-sum (second right-gate) num)
                       (is-full-carry (third right-gate) (1- num))
                       )
                      (and
                       (is-full-carry (second right-gate) (1- num))                     
                       (is-half-sum (third right-gate) num))))
                    )))))))
       (is-full-sum (node num)
         (if (= num 0)
             (is-half-sum node num)
             (let ((gate (gethash node graph)))
               (and
                (equal "XOR" (gate-type gate))
                (or
                 (and
                  (is-half-sum (second gate) num)
                  (is-full-carry (third gate) (1- num)))
                 (and
                  (is-half-sum (third gate) num)
                  (is-full-carry (second gate) (1- num)))))))))

    ;; (loop for node being the hash-keys of graph using (hash-value gate) 
    ;;       when (and (is-gate gate)
    ;;                 (equal "XOR" (first gate))
    ;;                 (or
    ;;                  (and
    ;;                   (is-half-sum (second gate) 5)
    ;;                   (is-full-carry (third gate) 4))
    ;;                  (and
    ;;                   (is-half-sum (third gate) 5)
    ;;                   (is-full-carry (second gate) 4))))
    ;;         do
    ;;            (format t "~A ~A~%" node gate))
    ;; FINDS jst / z05
    
    (rotatef (gethash "z05" graph) (gethash "jst" graph))
    
    ;; (loop for node being the hash-keys of graph using (hash-value gate)
    ;;       when (is-half-sum node 10)
    ;;         do (format t "~A ~A~%" node gate))
    ;; Finds gdf to swap for either mcm or tdw
    (rotatef (gethash "gdf" graph) (gethash "mcm" graph))

    ;; dnt appears as full sum for z15
    (rotatef (gethash "dnt" graph) (gethash "z15" graph))

    ;; z30 is wrong
    ;; (print (loop for node being the hash-keys of graph using (hash-value gate)
    ;;              collect
    ;;              (list node (is-full-sum node 30))))    

    (rotatef (gethash "gwc" graph) (gethash "z30" graph))

    
    (loop for i from 0 to 44
          always
          (let ((z (format nil "z~2,'0D" i)))
            (is-full-sum z i)))))

(defun part-2 (lines)
  (multiple-value-bind (starts graph-lines) (parse-input lines)
    (let ((graph (build-graph starts graph-lines)))
      (fix-graph graph)
      (multiple-value-bind (x y) (get-inputs graph)
        (when (= (+ x y) (get-output graph))
          (format t "~{~A~^,~}~%"
                  (sort (list "z05" "jst" "gdf" "mcm" "dnt" "z15" "gwc" "z30") #'string<)))))))

;; (part-2 (aoc/read-input-list 24 :real)) ; dnt,gdf,gwc,jst,mcm,z05,z15,z30


