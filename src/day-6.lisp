(defpackage :day-6
  (:import-from :alexandria :copy-array)
  (:use :cl :april :ppcre :utils)
  (:export :part-1 :part-2))

(in-package :day-6)

(proclaim '(optimize (debug 3) (speed 0)))

(defun find-start (input)
  (mapcar #'1- (coerce  (april/x input "⊃{(,⍵)⌿,⍳⍴⍵}'^'⍷x") 'list)))


(defun next-pos (pos heading)
  (let ((next-position (copy-list pos)))
    (cond ((eq heading 'up) (decf (first next-position)))
          ((eq heading 'down) (incf (first next-position)))
          ((eq heading 'left) (decf (second next-position)))
          ((eq heading 'right) (incf (second next-position))))
    next-position))

(defun check-move (board next-pos)
  (destructuring-bind (max-y max-x) (array-dimensions board)
    (destructuring-bind (y x) next-pos
      (if (or
           (< x 0)
           (>= x max-x)
           (< y 0)
           (>= y max-y))
          'done
          (let ((next-char (aref board y x)))
            (if (char= next-char #\#)
                'rotate
                'move))))))

(defun rotate (heading)
  (cond
    ((eq heading 'up) 'right)
    ((eq heading 'right) 'down)
    ((eq heading 'down) 'left)
    ((eq heading 'left) 'up)))

(defun run (board initial-position initial-heading)
  (let* ((position initial-position)
         (heading initial-heading)
         (path (list `(,@initial-position ,initial-heading))))
    (do*
     ((next-position (next-pos position heading) (next-pos position heading))
      (next-move (check-move board next-position) (check-move board next-position)))
     ((eq next-move 'done) (setf (aref board (first position) (second position)) #\X))
      (if (eq next-move 'rotate)
          (progn
            (setf heading (rotate heading)))
          (destructuring-bind (y x) position
            (setf (aref board y x) #\X)
            (setf path (cons `(,@next-position ,heading) path))
            (setf position next-position))))
    (nreverse path)))

(defun part-1 (input)
  (let ((board (copy-array input))
        (start (find-start input))
        (heading 'up))
    (let ((path (run board start heading)))
      (values 
       (april/x board
         "+/∊'X'=x")
       path))))

;; (part-1 (aoc/read-matrix 6 :example)) ; => 41 (6 bits, #x29, #o51, #b101001)
;; (part-1 (aoc/read-matrix 6 :real)) ; => 4973 (13 bits, #x136D)

(defun check-loop (board initial-position initial-heading &optional debug)
  (let* ((position initial-position)
         (heading initial-heading)
         (path (list `(,@initial-position ,initial-heading)))
         (path-hash (make-hash-table :test 'equal)))
    (let ((has-loop
            (do*
             ((next-position (next-pos position heading) (next-pos position heading))
              (next-move (check-move board next-position) (check-move board next-position)))
             ((eq next-move 'done) (setf (aref board (first position) (second position)) #\X))
              (when debug
                (format t "~A ~A ... ~A ~A ~%" position heading next-position next-move))
              (if (eq next-move 'rotate)
                  (setf heading (rotate heading))
                  (destructuring-bind (y x) position
                    (setf (aref board y x) #\X)
                    (setf path (cons `(,@next-position ,heading) path))
                    (if (null (gethash `(,@next-position ,heading) path-hash))
                        (setf (gethash `(,@next-position ,heading) path-hash) 1)
                        (incf (gethash `(,@next-position ,heading) path-hash)))
                    (when (> (gethash `(,@next-position ,heading) path-hash) 1)
                      (return t))
                    (setf position next-position))))))
      (eq has-loop t))))

(defun part-2 (input)
  (multiple-value-bind (board1 path) (part-1 input)
    (let ((board (copy-array input))
          (position (find-start input))
          (heading 'up)
          (uniques (make-hash-table :test 'equal)))
      (loop for (y x h) in (subseq path 1) do
        (setf (aref board y x) #\#)
        (let ((loops (check-loop board position heading)))
          (when loops
            (if (gethash (list y x) uniques)
                (incf (gethash (list y x) uniques))
                (setf (gethash (list y x) uniques) 1))))
        (setf (aref board y x) #\.))
      (loop for k being the hash-keys of uniques count k))))

;; (part-2 (aoc/read-matrix 6 :example)) ; => 6 (3 bits, #x6, #o6, #b110)
;; (part-2 (aoc/read-matrix 6 :real)) ; => 1482 (11 bits, #x5CA)

