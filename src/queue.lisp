(defpackage :my-queue
  (:use :cl)
  (:export :queue :emptyp :enqueue :dequeue))

(in-package :my-queue)

(defclass queue ()
  ((in-stack :initform nil :accessor in-stack)
   (out-stack :initform nil :accessor out-stack)))

(defmethod emptyp ((q queue))
  (with-slots (in-stack out-stack) q
    (eq t (and (null in-stack) (null out-stack)))))

(defmethod enqueue ((q queue) x)
  (with-slots (in-stack) q
    (push x in-stack)))

(defmethod dequeue ((q queue) )
  (with-slots (in-stack out-stack) q
    (cond
      ((emptyp q) nil)
      ((null out-stack) (progn
                          (setq out-stack (nreverse in-stack))
                          (setq in-stack nil)
                          (pop out-stack)))
      (t (pop out-stack)))))

(defmethod print-object ((object queue) stream)
  (with-slots (in-stack out-stack) object
    (format t "[~{~A ~}] [~{~A ~}]" in-stack out-stack)))
