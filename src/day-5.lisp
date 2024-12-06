(defpackage :day-5
  (:use :cl :utils :april :ppcre)
  (:export :part-1 :part-2))

(in-package :day-5)

(defun part-1 (input)
  ;; Brute force :)
  (destructuring-bind (rules-block updates-block) (ppcre:split "\\n\\n" input)
    (let ((rules
            (mapcar
             #'(lambda (rule-str)
                 (destructuring-bind (a b) (uiop:split-string rule-str :separator '(#\|))
                   (lambda (s)
                     (let ((match-a (multiple-value-list (ppcre:scan a s)))
                           (match-b (multiple-value-list (ppcre:scan b s))))
                       (if (or (null (first match-a)) (null (first match-b)))
                           ;; Missing either means the rule doesn't apply
                           t
                           ;; Otherwise, second must come after first
                           (> (elt match-b 0) (elt match-a 1))
                           )))))
             (remove-if #'uiop:emptyp
                        (uiop:split-string rules-block :separator '(#\newline)))))
          (updates (remove-if #'uiop:emptyp
                              (uiop:split-string updates-block :separator '(#\newline)))))
      (labels ((good-update-p (update) (every (lambda (rule) (funcall rule update)) rules))
               (middle (update-ints) (elt update-ints (floor (length update-ints) 2))))
        (apply #'+ (mapcar
                    (lambda (u)
                      (middle (mapcar #'parse-integer  (uiop:split-string u :separator '(#\,)))))
                    (remove-if-not #'good-update-p updates)))))))


;; (part-1 (aoc/read-input 5 :example)) ; => 143 (8 bits, #x8F, #o217, #b10001111)
;; (part-1 (aoc/read-input 5 :real)) ; => 5452 (13 bits, #x154C)


(defun part-2 (input)
  ;; Oops should've made a comparator
  (destructuring-bind (rules-block updates-block) (ppcre:split "\\n\\n" input)
    ;; Splitty split: by newlines then by whatever delimiter
    (let ((rules (loop for str in (remove-if #'uiop:emptyp
                                             (uiop:split-string rules-block :separator '(#\newline)))
                       collecting
                       (mapcar #'parse-integer (uiop:split-string str :separator '(#\|)))))
          (updates (loop for str in (remove-if #'uiop:emptyp
                                               (uiop:split-string updates-block :separator '(#\newline)))
                         collecting (mapcar #'parse-integer (uiop:split-string str :separator '(#\,))))))
      ;; If there's a rule like (91 20), this means that 91 comes before 20
      ;; This maps exactly to how common lisp comparators work - return non-nil
      ;;   if a comes strictly before b
      (labels ((comparator (a b)
                 (some (lambda (r) () (and (= (first r) a) (=  (second r) b))) rules))
               (middle (l) (elt l (floor (length l) 2))))
        ;; Brute force :)
        ;; In loop:
        ;;   Sort then see if sorted differs from original. If so, find middle of sorted.
        ;; Sum non-null results of loop
        (apply #'+ (remove-if #'null (loop for update in updates
                                           collect
                                           (let ((sorted (sort (copy-seq update) #'comparator)))
                                             (if (some #'null (map 'list #'= update sorted))
                                                 (middle sorted))))))))))

;; (part-2 (aoc/read-input 5 :example)) ; => 123 (7 bits, #x7B, #o173, #b1111011)
;; (part-2 (aoc/read-input 5 :real)) ; => 4598 (13 bits, #x11F6)

