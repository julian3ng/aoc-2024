(defpackage :day-4
  (:use :cl :april :utils)
  (:export :part-1 :part-2))

(in-package :day-4)


(defun part-1 (input)
  (let ((hv (april (with (:state :in ((x input))))
                   "+/∊(('XMAS'⍷x)+('SAMX'⍷x)+('XMAS'⍷⍉x)+('SAMX'⍷⍉x))"))
        (bslash (april (with (:state :in ((x input))))
                       "+/(('SAMX'∘≡)∨('XMAS'∘≡))¨,(⊂{1 1 ∘ ⍉ ⍵})⌺4 4⊢x") )
        (fslash (april (with (:state :in ((x input))))
                       "+/(('SAMX'∘≡)∨('XMAS'∘≡))¨,(⊂{1 1 ∘ ⍉ ⍵})⌺4 4⊢⌽x")))
    (+ hv bslash fslash)))

;; (part-1 (aoc/read-matrix 4 :example)) ; => 18 (5 bits, #x12, #o22, #b10010)
;; (part-1 (aoc/read-matrix 4 :real)) ; => 2530 (12 bits, #x9E2)

(defun part-2 (input)
  (april (with (:state :in ((x input))))
         ;; hahahaha what
         "+/(∧/((('MAS'∘≡)∨('SAM'∘≡))¨ ((1 1 ∘ ⍉)(,⍥⊂)(⌽⍉⍨1*⍴))))¨ ,(⊂⊢)⌺3 3⊢x"))

;; (part-2 (aoc/read-matrix 4 :example)) ; => 9 (4 bits, #x9, #o11, #b1001)
;; (part-2 (aoc/read-matrix 4 :real)) ; => 1921 (11 bits, #x781)
