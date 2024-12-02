(defpackage :day-1
  (:use :cl :april :utils)
  (:export :part-1 :part-2))

(in-package :day-1)

(defun part-1 (input)
  (april-f (with (:state :in ((x input))))
           ;; +/| absolute value of argument, then reduce with sum
           ;; (1⌷⍉)(...)(2⌷⍉) do the middle with the first and second columns as
           ;;   args
           ;; (-⍥((⊂⍋)⌷⊣)) dyadically: sort both args then subtract
           "+/|((1⌷⍉)(-⍥((⊂⍋)⌷⊣))(2⌷⍉))x"))

(defun part-2 (input)
  (april-f (with (:state :in ((x input))))
           ;; +/ sum of
           ;; ((1⌷⍉)×(f))x first column of x times f of x
           ;; +/∘↑ sum after mixing
           ;; (1⌷⍉)((⊃=)⍥⊂)(⊂2⌷⍉)
           ;;   - take first column and second column; enclose second column.
           ;;   - Enclose both, then find where column 2 = column 1. Massage
           ;;   - shape back into matrix.
           "+/((1⌷⍉)×(+/∘↑((1⌷⍉)((⊃=)⍥⊂)(⊂2⌷⍉))))x"))
