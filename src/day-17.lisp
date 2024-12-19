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
                (format t "~%"))
            (when (= op 3)
              (format t "================================~%")))
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
    (print-instructions instructions)
    (string-trim
     '(#\,)
     (coerce (loop for c across (with-output-to-string (s) (run-vm a b c instructions :stream s :logs t))
                   collect c) 'string))))

;; (part-1 (aoc/read-ints-ignoring-rest 17 :example)) ; => "4,6,3,5,6,3,5,2,1,0"
;; (part-1 (aoc/read-ints-ignoring-rest 17 :real)) ; => "7,3,0,5,7,1,4,0,5"

(defun run-vm-with-a (a instructions &key (logs nil))
  (coerce
   (mapcar
    #'digit-char-p
    (remove-if
     (lambda (c) (char= #\, c))
     (loop for c across (with-output-to-string (s) (run-vm a 0 0 instructions :stream s :logs logs)) collect c)))
   'vector))


(defun find-valid (instructions)
  (let* ((goals (loop for i from (1- (length instructions)) downto 0 collect (subseq instructions i)))
         (currents (list 0)))
    (loop for i from 0 to (1- (length goals)) do
      (setf currents (loop for A* in currents nconc (loop for A from (* A* 8) to (+ (* A* 8) 7) when (equalp (elt goals i) (run-vm-with-a A instructions)) collect A))))
    currents))


(defun part-2 (ints)
  (let ((instructions (fourth (parse-input ints))))
    (apply #'min (find-valid instructions))))

;; (part-2 (aoc/read-ints-ignoring-rest 17 :real)) ; => 202972175280682 (48 bits, #xB89A24682A2A)
