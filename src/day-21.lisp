(defpackage :day-21
  (:use :cl :utils :my-queue))

(in-package :day-21)

#|

789
456
123
 0A

NUMPAD:  029A
transitions: A0 02 29 9A
A0 = <A
02 = ^A
29 = ^^>A
9A = vvA

KEYPAD1: <A^A^^>AvvA
KEYPAD2: <<vA>>^A<A>A<AAv>A^A


|#

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


(let ((paths (make-paths *numpad*)))
  (loop for a across "A0123456789" do
    (loop for b across "A0123456789" do
      (let ((ab (coerce (list a b) 'string)))
        (format t "~A ~A: ~A~%" a b (gethash ab paths))))
    (format t "~%")))

(defun make-paths (board)
  (declare (optimize (debug 3)))
  (let ((paths (make-hash-table :test 'equal)))
    (destructuring-bind (my mx) (array-dimensions board)
      (loop for sy from 0 to (1- my) do
        (loop for sx from 0 to (1- mx) do
          (unless (null (aref board sy sx))
            (let ((q (make-instance 'queue))
                  (move-to-get-here (make-array (list my mx) :initial-element nil))
                  (prev (make-array (list my mx) :initial-element nil)))
              (labels ((set-move (p move) (setf (apply #'aref move-to-get-here p) move))
                       (set-prev (p pp) (setf (apply #'aref prev p) pp))
                       (in-bounds (p) (and
                                       (<= 0 (first p) (1- my))
                                       (<= 0 (second p) (1- mx))
                                       (not (null (apply #'aref board p)))))
                       (unvisited (p) (and
                                       (in-bounds p)
                                       (eq (apply #'aref move-to-get-here p) nil))))
                (set-move (list sy sx) #\.)
                (enqueue q (list sy sx))
                (loop until (emptyp q) do
                  (let ((cur (dequeue q)))
                    (destructuring-bind (y x) cur
                      (loop for dy in '(-1 0 1 0)
                            and dx in '(0 1 0 -1)
                            and move across "^>v<"
                            when (and dy dx)
                              do
                                 (let ((neighbor (list (+ y dy) (+ x dx))))
                                   (when (unvisited neighbor)
                                     (set-move neighbor move)
                                     (set-prev neighbor cur )
                                     (enqueue q neighbor))))))))
              ;; (when (equal (list sy sx) '(3 2))
              ;;   (loop for y from 0 to (1- my) do
              ;;     (loop for x from 0 to (1- mx) do
              ;;       (format t "~A " (aref prev y x)))
              ;;     (format t "~%"))
              ;;   (loop for y from 0 to (1- my) do
              ;;     (loop for x from 0 to (1- mx) do
              ;;       (format t "~A " (aref move-to-get-here y x)))
              ;;     (format t "~%"))
              ;;   )
              (let ((sc (aref board sy sx)))
                (loop for y from 0 to (1- my) do
                  (loop for x from 0 to (1- mx) do
                    (let ((c (aref board y x)))
                      (unless (or (null c) (null sc))
                        (let ((cur (list y x))
                              (s (list #\A)))
                          (loop until (null (apply #'aref prev cur)) do
                            (push (apply #'aref move-to-get-here cur) s)
                            (setf cur (apply #'aref prev cur)))
                          (setf (gethash (coerce (list sc c) 'string) paths)
                                (coerce s 'string)))))))))))))
    (setf (gethash "<A" paths) ">>^A") ;; special case I can't convince BFS of...
    paths))

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
                              (print line-with-a)
                              (print s-groups)
                              (print s)
                              (print ss-groups)
                              (print ss)
                              (print sss-groups)
                              (print sss)
                              (format t "~&~A * ~A = ~A~%" (parse-integer (subseq line-with-a 1) :junk-allowed t)
                                      (1- (length sss))
                                      (* (parse-integer (subseq line-with-a 1) :junk-allowed t)
                                         (1- (length sss))))
                              (* (parse-integer (subseq line-with-a 1) :junk-allowed t)
                                 (1- (length sss)))))))

(part-1 (aoc/read-input-list 21 :example))
(part-1 (aoc/read-input-list 21 :real))

(defun part-2 (input)
  (let ((numpad-paths (make-numpad-paths))
        (dpad-paths (make-dpad-paths)))
    (loop for line in input
          sum
          (let ((cur-line (expand-path (concatenate 'string "A" line) numpad-paths)))
            (loop repeat 3 do
              (setf cur-line (expand-path (apply #'concatenate 'string "A" cur-line) numpad-paths)))
            
            (* (parse-integer line :junk-allowed t)
               (1- (length cur-line)))))))

(part-2 (aoc/read-input-list 21 :real))



