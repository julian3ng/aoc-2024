(defpackage :day-20
  (:use :cl :utils :april)
  (:export :part-1 :part-2))

(in-package :day-20)

(defun find-start-end (board)
  (destructuring-bind (my mx) (array-dimensions board)
    (let ((start nil)
          (end nil))
      (loop for y from 0 to (1- my) do
        (loop for x from 0 to (1- mx) do
          (when (char= (aref board y x) #\S)
            (setf start (list y x)))
          (when (char= (aref board y x) #\E)
            (setf end (list y x)))          
              ))
      (values start end))))

(defun bfs (board start end)
  (declare (optimize (debug 3)))
  (let ((dists (make-array (array-dimensions board)
                           :element-type 'integer
                           :initial-element -1))
        (path nil))
    (labels ((set-dist (p dist) (setf (apply #'aref dists p) dist))
             (empty (p) (or (char= (apply #'aref board p) #\.)
                             (char= (apply #'aref board p) #\E)))
             (unvisited (p) (= (apply #'aref dists p) -1)))
      (let ((prev nil)
            (cur start)
            (max-dist 0))
        (loop for dist from 0
              until (char= (apply #'aref board cur) #\E)
              do
                 (set-dist cur dist)
                 (setf max-dist dist)
                 (destructuring-bind (y x) cur
                   (loop for dy in '(-1 0 1 0)
                         and dx in '(0 1 0 -1)
                         when (and (not (null dy)) (not (null dx)))
                           do
                              (let ((neighbor (list (+ y dy) (+ x dx))))
                                (when (and (empty neighbor) (unvisited neighbor))
                                  (push cur path)
                                  (shiftf prev cur neighbor))))))
        (set-dist end (1+ max-dist))))
    (values dists (nreverse path))))

(defun check-shortcuts (dists path)
  (labels ((get-dist (p) (apply #'aref dists p))
           (in-bounds (p)
             (destructuring-bind (my mx) (array-dimensions dists) 
               (and (<= 0 (first p) (1- my))
                    (<= 0 (second p) (1- mx))))))
    (let ((cheats (make-hash-table :test 'equal))
          ;; time saved : count
          (saves (make-hash-table)))
      (loop for p in path do
        (loop for direction in '((-1 0) (0 1) (1 0) (0 -1))
              do
                 (let ((neighbor (mapcar #'+ p direction))
                       (thru (mapcar #'+ p direction direction)))
                   (when (in-bounds thru)
                     (let ((d1 (get-dist p))
                           (d2 (get-dist neighbor))
                           (d3 (get-dist thru)))
                       (when (and (> d3 d1 -1) (= d2 -1))
                         (let ((save (- (- d3 d1) 2)))
                           (setf (gethash neighbor cheats) save)
                           (if (gethash save saves)
                               (incf (gethash save saves))
                               (setf (gethash save saves) 1)))))))))
      (values cheats saves))))

(defun part-1 (input-matrix)
  (multiple-value-bind (start end) (find-start-end input-matrix)
    (multiple-value-bind (dists path) (bfs input-matrix start end)
      (multiple-value-bind (cheats saves)(check-shortcuts dists path)
        (loop for k being the hash-keys of saves using (hash-value v)
              when (>= k 100) sum v)))))

;; (part-1 (aoc/read-matrix 20 :real)) ; => 1384 (11 bits, #x568)

(defun point2d-p (thing)
  (and (consp thing)
       (= (length thing) 2)
       (every #'integerp thing)))

(deftype point2d () `(satisfies point2d-p ))



(defun in-bounds (p bounds)
  (destructuring-bind (my mx) bounds
    (destructuring-bind (y x &optional _) p
      (declare (ignore _))
      (and (<= 0 y (1- my))
           (<= 0 x (1- mx))))))

(defun diamond-points (p diameter &optional bounds)
  (destructuring-bind (y x) p
    (let ((points (nconc
                   (loop for dx from (- 0 diameter) to diameter
                         when (/= dx 0)
                           collect (list y (+ x dx) (abs dx)))
                   (loop for dy from 1 to diameter
                         nconc (loop for dx from (- dy diameter) to (- diameter dy)
                                     collect
                                     (list (+ y dy) (+ x dx) (+ (abs dy) (abs dx)))))
                   (loop for dy from 1 to diameter
                         nconc (loop for dx from (- dy diameter) to (- diameter dy)
                                     collect
                                     (list (- y dy) (+ x dx) (abs (+ (abs dy) (abs dx)))))))))
      (if (null bounds)
          points
          (remove-if-not (lambda (pp) (in-bounds pp bounds)) points)))))

(defun part-2 (input-matrix)
  ;; loop through path, point = p
  ;; look at everything in a range 20 diamond around p
  ;; if x is some point in that diamond that is in bounds, on the path, and
  ;; has path distance >= 100, count it
  (multiple-value-bind (start end) (find-start-end input-matrix)
    (multiple-value-bind (dists path) (bfs input-matrix start end)
      (loop for p in path sum
        (loop for dp in (diamond-points p 20 (array-dimensions dists))
              count
              (let ((d1 (apply #'aref dists p))
                    (d2 (apply #'aref dists (subseq dp 0 2))))
                (and (> d2 d1 -1) (>= (- (- d2 d1) (third dp)) 100))))))))


;; (part-2 (aoc/read-matrix 20 :real)) ; => 1008542 (20 bits, #xF639E)
