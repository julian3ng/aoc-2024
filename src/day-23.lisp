(defpackage :day-23
  (:use :cl :utils))


(in-package :day-23)

(defun parse-input (lines)
  (loop for line in lines collect (uiop:split-string line :separator "-")))

(defun make-graph (edges)
  (let ((graph (make-hash-table :test 'equal)))
    (loop for (u v) in edges do
      (push v (gethash u graph nil))
      (push u (gethash v graph nil)))
    graph))

(defun print-graph (graph)
  (maphash
   (lambda (v edges) (format t "~A: ~{~A~^, ~}~%" v edges))
   graph))

(defun adjacentp (u v graph)
  (not (null (find u (gethash v graph) :test 'equal))))

(defun triangles (u graph)
  (let ((neighbors (gethash u graph)))
    (remove-if #'null
               (loop for v in neighbors
                     for i from 0
                     nconc
                     (loop for w in neighbors
                           for j from 0 to i
                           collect
                           (when (and (string/= v w) (adjacentp v w graph))
                             (sort (list u v w) #'string<)))))))

(defun part-1 (input)
  (let ((graph (make-graph (parse-input input)))
        (uniques (make-hash-table :test 'equal)))
    (loop for v being the hash-keys of graph
          when (char= (aref v 0) #\t)
            do
               (loop for tri in (triangles v graph) do (incf (gethash tri uniques 0))))
    (hash-table-count uniques))
  )

;; (part-1 (aoc/read-input-list 23 :example)) ; => 7 (3 bits, #x7, #o7, #b111)
;; (part-1 (aoc/read-input-list 23 :real)) ; => 1149 (11 bits, #x47D)

(defun expand-clique (c graph)
  (let ((uniques (make-hash-table :test 'equal)))
    (loop for u being the hash-keys of graph do
      (when (loop for v in c always (adjacentp u v graph))
        (incf (gethash (sort (cons u (copy-seq c)) #'string<) uniques 0))))
    (loop for k being the hash-keys of uniques collect k)))

(defun cliques (n graph)
  "Find all cliques of size n containing in graph"
  (let ((uniques (make-hash-table :test 'equal)))
    (case n
      (1 (loop for v being the hash-keys of graph do (incf (gethash v uniques 0))))
      (2 (loop for u being the hash-keys of graph
               do
                  (loop for v in (gethash u graph)
                        do (incf (gethash (sort (list u v) #'string<) uniques 0)))))
      (3 (loop for v being the hash-keys of graph do
        (loop for tri in (triangles v graph) do (incf (gethash tri uniques 0)))))
      (t
       (let ((last-cliques (cliques (1- n) graph)))
         (loop for clique in last-cliques do
           (loop for u being the hash-keys of graph do
             (when (loop for v in clique always (adjacentp u v graph))
               (incf (gethash (sort (cons u (copy-seq clique)) #'string<) uniques 0))))))))
    (loop for k being the hash-keys of uniques collect k)))


(defun maximize-clique (c graph)
  (let ((clique-so-far (copy-seq c)))
    (loop for u being the hash-keys of graph do
      (when (loop for v in clique-so-far always (adjacentp u v graph))
        (push u clique-so-far)))
    clique-so-far))

(defun part-2 (input)
  (let* ((parsed (parse-input input))
         (graph (make-graph parsed)))
    (loop for edge in parsed
          with best-length = 0
          with best = nil
          do (let ((maxed (maximize-clique edge graph)))
               (when (> (length maxed) best-length)
                 (setf best-length (length maxed))
                 (setf best maxed)))
          finally (return (format nil "~{~A~^,~}" (sort best #'string<))))))


(part-2 (aoc/read-input-list 23 :real)) ; => "as,co,do,kh,km,mc,np,nt,un,uq,wc,wz,yo"
