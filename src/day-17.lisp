(defpackage :day-17
  (:use :cl :utils)
  (:export :part-1 :part-2))

(in-package :day-17)

(defun parse-input (ints)
  (destructuring-bind ((a) (b) (c) nil instructions) ints
    (list a b c (coerce instructions 'vector))))

(defun opname (op)
  (cond
    ((= op 0) "[adv]")
    ((= op 1) "bxl")
    ((= op 2) "[bst]")
    ((= op 3) "jnz")
    ((= op 4) "bxc")
    ((= op 5) "[out]")
    ((= op 6) "[bdv]")
    ((= op 7) "[cdv]")))

(defun run-vm (a b c instructions &key (stream t) (logs nil) (cb nil))
  (let ((ra a)
        (rb b)
        (rc c)
        (ip 0)
        (l (length instructions)))
    (labels
        ((combo (arg)
           (cond
             ((= arg 4) ra)
             ((= arg 5) rb)
             ((= arg 6) rc)
             ((= arg 7) (error "Invalid combo arg 7"))
             (t arg))))
      (when (not (null logs)) (format t "~8A || ~8A | ~8A | ~8A~%" "IP" "A" "B" "C"))
      (loop while (< ip l) do
        (let ((op (aref instructions ip))
              (arg (aref instructions (1+ ip))))
          (when (not (null logs))
            (format t "~8A || ~8A | ~8A | ~8A || ~8A ~8A (~8A)" ip ra rb rc (opname op) arg (combo arg))
            (if (= op 5)
                (format t " ==> ~A~%" (mod (combo arg) 8))
                (format t "~%")))
          (cond
            ;; adv
            ((= op 0) (setf ra (floor ra (expt 2 (combo arg)))))
            ;; bxl
            ((= op 1) (setf rb (logxor rb arg)))
            ;; bst
            ((= op 2) (setf rb (mod (combo arg) 8)))
            ;; jnz
            ((= op 3) (when (/= ra 0) (setf ip (- arg 2)))) ; -2 so that incf ip brings it to arg
            ;; bxc
            ((= op 4) (setf rb (logxor rb rc)))
            ;; out
            ((= op 5) (format stream "~A," (mod (combo arg) 8)))
            ;; bdv
            ((= op 6) (setf rb (floor ra (expt 2 (combo arg)))))
            ;; cdv
            ((= op 7) (setf rc (floor ra (expt 2 (combo arg))))))
          (incf ip 2))))
    (when cb (funcall cb ra rb rc instructions))))

(defun print-instructions (instructions)
  (format t "~&")
  (loop for i from 0 to (1- (length instructions)) 
        do
           (let ((thing (aref instructions i)))
             (if (zerop (mod i 2))
                 (format t "~A " (opname thing))
                 (format t "~A " thing))))
  (format t "~%"))

(defun part-1 (ints)
  (destructuring-bind (a b c instructions) (parse-input ints)
    (string-trim
     '(#\,)
     (coerce (loop for c across (with-output-to-string (s) (run-vm a b c instructions :stream s :logs t))
                               collect c) 'string))))

;; (part-1 (aoc/read-ints-ignoring-rest 17 :example)) ; => "4,6,3,5,6,3,5,2,1,0"
;; (part-1 (aoc/read-ints-ignoring-rest 17 :real)) ; => "7,3,0,5,7,1,4,0,5"


(defun part-2 (ints)
  (let ((s (part-1 ints)))
    
    ))

#|
digit 1 is 2,

A = SOMETHING                                        ;
B = (A % 8) ^ 1                         ;
C = trunc(A / 2 ^ B)
B = B ^ C
A = A / 8
B = B ^ 4
PRINT B % 8

A = SOMETHING
((((A % 8) ^ 1) ^ trunc(A / (2^((A % 8) ^ 1)))) ^ 4) % 8
A = A / 8

|#

(defun print-digit (A)
  (print
   (mod
    (logxor
     (logxor
      (logxor (mod A 8) 1)
      (floor A (expt 2 (logxor (mod A 8) 1))))
     4)
    8
    )))

(print-digit 28066687)
(print-digit (floor 28066687 8))
(print-digit (floor (floor 28066687 8) 8))
(print-digit (floor (floor (floor 28066687 8) 8) 8))



;; (loop for A = 28066687 do
;;       (print-digit A)
;;       (setf A (floor A 8)))
