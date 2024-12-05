(defpackage :day-4
  (:use :cl :april :utils)
  (:export :part-1 :part-2))

(in-package :day-4)


(defun part-1 (input)
  (let ((hv (april (with (:state :in ((x input))))
                   ;; +/∊ enlist then sum
                   ;; ('XMAS'⍷x)+... returns a bit vector with 1s where 'XMAS'
                   ;; exists in x
                   ;; Same applies to SAMX, and we do the same thing with the
                   ;; transpose. These account for horizontal and vertical XMAS
                   ;; instances, both forwards and backwards.
                   "+/∊(('XMAS'⍷x)+('SAMX'⍷x)+('XMAS'⍷⍉x)+('SAMX'⍷⍉x))"))
        (bslash (april (with (:state :in ((x input))))
                       ;; +/ sum
                       ;; (('SAMX'∘≡)∨('XMAS'∘≡))¨ match XMAS or SAMX on each of the following
                       ;; (⊂{1 1 ∘ ⍉ ⍵}) get the main diagonal of the argument, and enclose it
                       ;; (...)⌺4 4 ⊢ x apply ... to each 4x4 window of x
                       "+/(('SAMX'∘≡)∨('XMAS'∘≡))¨,(⊂{1 1 ∘ ⍉ ⍵})⌺4 4⊢x") )
        (fslash (april (with (:state :in ((x input))))
                       ;; Same thing, but on vertical mirror of x to get the
                       ;; opposite diagonals
                       "+/(('SAMX'∘≡)∨('XMAS'∘≡))¨,(⊂{1 1 ∘ ⍉ ⍵})⌺4 4⊢⌽x")))
    (+ hv bslash fslash)))

;; (part-1 (aoc/read-matrix 4 :example)) ; => 18 (5 bits, #x12, #o22, #b10010)
;; (part-1 (aoc/read-matrix 4 :real)) ; => 2530 (12 bits, #x9E2)

(defun part-2 (input)
  (april (with (:state :in ((x input))))
         ;; hahahaha what
         ;; Right to left
         ;; ,(⊂⊢)⌺3 3 ⊢ x  Get 3x3 windows of x, and put them into a vector
         ;; ...((1 1 ∘ ⍉)(,⍥⊂)(⌽⍉⍨1*⍴)).. Get the main- and anti-diagonals
         ;; ...(('MAS'∘≡)∨('SAM'∘≡))¨... Check if a diagonal matches with SAM or MAS
         ;; ^/(......)¨ Check if both diagonals match with SAM or MAS
         ;; +/(...) Count all 3x3 windows that form X-MASes
         "+/(∧/((('MAS'∘≡)∨('SAM'∘≡))¨ ((1 1 ∘ ⍉)(,⍥⊂)(⌽⍉⍨1*⍴))))¨ ,(⊂⊢)⌺3 3⊢x"))

;; (part-2 (aoc/read-matrix 4 :example)) ; => 9 (4 bits, #x9, #o11, #b1001)
;; (part-2 (aoc/read-matrix 4 :real)) ; => 1921 (11 bits, #x781)
