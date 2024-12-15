(defpackage :day-12
  (:use :cl :ppcre :april :utils)
  (:export :part-1 :part-2))

(in-package :day-12)

(defun neighbors-4 (p bounds)
  (destructuring-bind (my mx) bounds
    (destructuring-bind (y x) p
      (labels ((in-bounds (y x)
                 (and (<= 0 y (1- my))
                      (<= 0 x (1- mx)))))
        (loop for (i j) in '((-1 0) (0 1) (1 0) (0 -1))
              when (in-bounds (+ y i) (+ x j))
                collect (list (+ y i) (+ x j)))))))

(defun dfs (board)
  (let* ((bounds (array-dimensions board))
         (visited (make-array bounds :initial-element 0))
         (score 0))
    (labels ((visit (p) (destructuring-bind (y x) p
                          (setf (aref visited y x) 1)))
             (visited (p) (destructuring-bind (y x) p
                            (not (zerop (aref visited y x)))))
             (explore (p)
               (visit p)
               (let ((my-area 1)
                     (my-perimeter 0))
                 (destructuring-bind (y x) p
                   ;; (format t "Exploring: ~A = ~A~%" p (aref board y x))
                   
                   (loop for neighbor in (neighbors-4 p bounds)
                         when (and (char= (aref board y x) (aref board (first neighbor) (second neighbor)))
                                   (not (visited neighbor)))
                           do (multiple-value-bind (area perimeter) (explore neighbor)
                                (incf my-area area)
                                (incf my-perimeter perimeter)))
                   (loop for neighbor in (neighbors-4 p bounds)
                         when (char/= (aref board y x) (aref board (first neighbor) (second neighbor)))
                           do (incf my-perimeter))
                   (incf my-perimeter (- 4 (length (neighbors-4 p bounds)))))

                 (values my-area my-perimeter))))
      (loop for i from 0 to (1- (first bounds)) do
        (loop for j from 0 to (1- (second bounds))
              when (not (visited (list i j)))
                do
                   (multiple-value-bind (area perimeter) (explore (list i j))
                     ;; (format t "~A: ~A*~A=~A~%" (aref board i j) area perimeter (* area perimeter))
                     (incf score (* area perimeter)))))
      score)))

(defun part-1 (input)
  (dfs input))

;; (part-1 (aoc/read-matrix 12 :example)) ; => 1930 (11 bits, #x78A)
;; (part-1 (aoc/read-matrix 12 :real)) ; => 1421958 (21 bits, #x15B286)

#|

X = 4

XX = 2 + 2

XXX = 2 + 0 + 2

XX = 2 + 2 + 2
X

XX
XX = 1 + 1 + 1 + 1

XX
XX = 2 + 2 + 2 + 2

XXX
X  = 2 + 0 + 2 + 2

|#

(let ((p '(3 2)))
  )


(defun dfs-2 (board)
  (let* ((bounds (array-dimensions board))
         (visited (make-array bounds :initial-element 0))
         (score 0))
    (labels ((in-bounds (p) (destructuring-bind (y x) p
                              (and (<= 0 y (1- (first bounds)))
                                   (<= 0 x (1- (second bounds))))))
             (count-corners (p) (let ((coord-checks  (loop for (p1 p2)
                                                             on '((-1 0) (0 1) (1 0) (0 -1) (-1 0))
                                                           when p2
                                                             collect (list (mapcar #'+ p1 p)
                                                                           (mapcar #'+ p2 p)
                                                                           (mapcar #'+ p1 p2 p))))
                                      (pc (apply #'aref board p)))
                                  ;; for each adjacent pair of neighbors
                                  (loop for (p1 p2 p3) in coord-checks
                                        count
                                        (let* ((p1c (if (in-bounds p1) (apply #'aref board p1) #\Space))
                                               (p2c (if (in-bounds p2) (apply #'aref board p2) #\Space))
                                               (p3c (if (in-bounds p3) (apply #'aref board p3) #\Space))
                                               (outer (and (char/= p1c pc)
                                                           (char/= p2c pc)))
                                               (inner (and (char= p1c p2c pc)
                                                           (char/= pc p3c))))
                                          (prog1 (or outer inner)
                                            ;; (format t "~A=~A: ~A=~A & ~A=~A ~A=~A? O: ~A, I: ~A~%"
                                            ;;         p pc p1 p1c p2 p2c p3 p3c outer inner)
                                            )))))
             (visit (p) (destructuring-bind (y x) p
                          (setf (aref visited y x) 1)))
             (visited (p) (destructuring-bind (y x) p
                            (not (zerop (aref visited y x)))))
             (explore (p)
               (visit p)
               (let ((my-area 1)
                     (my-sides 0))
                 (destructuring-bind (y x) p
                   (loop for neighbor in (neighbors-4 p bounds)
                         when (and (char= (aref board y x) (aref board (first neighbor) (second neighbor)))
                                   (not (visited neighbor)))
                           do (multiple-value-bind (area sides) (explore neighbor)
                                (incf my-area area)
                                (incf my-sides sides)))
                   (incf my-sides (count-corners p)))
                 ;;                 (format t "~A: ~A~%" p my-sides)
                 (values my-area my-sides))))
      (loop for i from 0 to (1- (first bounds)) do
        (loop for j from 0 to (1- (second bounds))
              when (not (visited (list i j)))
                do
                   (multiple-value-bind (area sides) (explore (list i j))
                     ;;                     (format t "~A: ~A*~A=~A~%" (aref board i j) area sides (* area sides))
                     (incf score (* area sides)))))
      score)))

(defun part-2 (input)
  (dfs-2 input))

;; (time (part-2 (aoc/read-matrix 12 :example))) ; => 1206 (11 bits, #x4B6)
;; (time (part-2 (aoc/read-matrix 12 :real))) ; => 885394 (20 bits, #xD8292)
