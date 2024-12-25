(defpackage :day-21
  (:use :cl :utils :my-queue))

(in-package :day-21)

(defparameter *numpad* (make-array '(4 3)
                                   :initial-contents
                                   '((#\7 #\8 #\9)
                                     (#\4 #\5 #\6)
                                     (#\1 #\2 #\3)
                                     (nil #\0 #\A))))

(defparameter *dpad* (make-array '(2 3)
                                 :initial-contents
                                 '((nil #\^ #\A)
                                   (#\< #\v #\>))))

(defun expand-path (path paths)
  (loop for i from 0 to (- (length path) 2) collect (gethash (subseq path i (+ i 2)) paths)))


(defun do-presses-on (path pad)
  (let ((cur (destructuring-bind (my mx) (array-dimensions pad)
               (loop for y from 0 to (1- my) nconc
                 (loop for x from 0 to (1- mx) when (equal (aref pad y x) #\A)
                       nconc (list y x))))))
    (let ((s nil))
      (loop for move across path do
        (case move
          (#\^ (decf (first cur)))
          (#\v (incf (first cur)))
          (#\< (decf (second cur)))
          (#\> (incf (second cur)))
          (#\A (let ((button (apply #'aref pad cur)))
                 (push (apply #'aref pad cur) s)
                 (when (char= button #\A)
                   (push #\Space s))))
          (otherwise nil)))
      (coerce (nreverse s) 'string))))


(defun make-numpad-paths ()
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "A4" h) "^^<<A")
    (setf (gethash "48" h) "^>A")
    (setf (gethash "80" h) "vvvA")
    (setf (gethash "0A" h) ">A")

    (setf (gethash "A6" h) "^^A")
    (setf (gethash "68" h) "<^A")
    (setf (gethash "82" h) "vvA")
    (setf (gethash "2A" h) "v>A")

    (setf (gethash "A1" h) "^<<A")
    (setf (gethash "14" h) "^A")
    (setf (gethash "40" h) ">vvA")

    (setf (gethash "A2" h) "<^A")
    (setf (gethash "24" h) "<^A")
    (setf (gethash "46" h) ">>A")
    (setf (gethash "6A" h) "vvA")

    (setf (gethash "A9" h) "^^^A")
    (setf (gethash "93" h) "vvA")
    (setf (gethash "38" h) "<^^A")
    (setf (gethash "8A" h) "vvv>A")
    h))

(defun make-dpad-paths ()
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "AA" h) "A"
          (gethash "A<" h) "v<<A"
          (gethash "Av" h) "<vA"
          (gethash "A^" h) "<A"
          (gethash "A>" h) "vA"

          (gethash "<A" h) ">>^A"
          (gethash "<<" h) "A"
          (gethash "<v" h) ">A"
          (gethash "<^" h) ">^A"
          (gethash "<>" h) ">>A"

          (gethash "vA" h) "^>A"
          (gethash "v<" h) "<A"
          (gethash "vv" h) "A"
          (gethash "v^" h) "^A"          
          (gethash "v>" h) ">A"

          (gethash "^A" h) ">A"
          (gethash "^<" h) "v<A"
          (gethash "^v" h) "vA"
          (gethash "^^" h) "A"          
          (gethash "^>" h) "v>A"

          (gethash ">A" h) "^A"
          (gethash "><" h) "<<A"
          (gethash ">v" h) "<A"
          (gethash ">^" h) "<^A"          
          (gethash ">>" h) "A")
    h))

(defun part-1 (input)
  (let ((numpad-paths (make-numpad-paths))
        (dpad-paths (make-dpad-paths)))
    (loop for line in input sum
                            (let* ((line-with-a (concatenate 'string "A" line))
                                   (s-groups  (expand-path line-with-a numpad-paths ))
                                   (s  (apply #'concatenate 'string "A" s-groups))
                                   (ss-groups (expand-path s dpad-paths))
                                   (ss (apply #'concatenate 'string "A" ss-groups))
                                   (sss-groups (expand-path ss dpad-paths))
                                   (sss (apply #'concatenate 'string "A" sss-groups)))
                              (* (parse-integer (subseq line-with-a 1) :junk-allowed t)
                                 (1- (length sss)))))))

;; (part-1 (aoc/read-input-list 21 :real)) ; => 176452 (18 bits, #x2B144)

(defun seq-length (moves layers)
  (let ((numpad-paths (make-numpad-paths))
        (dpad-paths (make-dpad-paths))
        (memo (make-hash-table :test 'equalp)))
    (labels ((dp (key)
               (let ((maybe (gethash key memo)))
                 (if maybe
                     maybe
                     (destructuring-bind (moves cur-layer) key
                       (let ((moves-plus-a (concatenate 'string "A" moves)))
                         (cond
                           ((zerop cur-layer) (setf (gethash key memo) (loop for i from 0 to (- (length moves-plus-a) 2)
                                                                             sum (dp (list (apply #'concatenate 'string (expand-path (subseq moves-plus-a i (+ i 2)) numpad-paths))
                                                                                           (1+ cur-layer))))))
                           ((>= cur-layer layers) (setf (gethash key memo) (length moves)))
                           (t (setf (gethash key memo) (loop for i from 0 to (- (length moves-plus-a) 2)
                                                             sum (dp (list (apply #'concatenate 'string (expand-path (subseq moves-plus-a i (+ i 2)) dpad-paths))
                                                                           (1+ cur-layer))))))))
                       (gethash key memo))))))

      (dp (list moves 0)))))

(defun score (input)
  (*  (parse-integer input :junk-allowed t) (seq-length input 26)))

(defun part-2 (input)
  (loop for line in input sum (score line )))

(part-2 (aoc/read-input-list 21 :real)) ; => 218309335714068 (48 bits, #xC68D1A680914)

