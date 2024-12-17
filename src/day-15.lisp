(defpackage :day-15
  (:use :cl :april :ppcre :alexandria :utils)
  (:export :part-1 :part-2))

(in-package :day-15)

(defun split-input (input-list)
  (declare (optimize (debug 3)))
  (let* ((map-lines (loop for line in input-list
                          until (equal "" line)
                          collect
                          line))
         (directions (apply #'concatenate 'string (loop for line in (subseq input-list (1+ (length map-lines)))
                                                        collect
                                                        line))))
    (values (make-array (list (length map-lines) (length (first map-lines)))
                        :initial-contents map-lines)
            directions)))

(defun find-player (initial-board)
  (april/x initial-board "1-⍨⊃⍸'@'=x"))

(defun simulate (board directions)
  (destructuring-bind (my mx) (array-dimensions board)
    (let ((position (find-player board)))
      (labels ((find-non-rock (move-char)
                 (cond
                   ((char= move-char #\<)
                    (let ((pos (loop for j from (1- (aref position 1)) downto 0
                                     when (char/= (aref board (aref position 0) j) #\O)
                                       return (list (aref position 0) j))))
                      pos))
                   ((char= move-char #\>)
                    (let ((pos (loop for j from (1+ (aref position 1)) to (1- mx)
                                     when (char/= (aref board (aref position 0) j) #\O)
                                       return (list (aref position 0) j))))
                      pos))
                   ((char= move-char #\^)
                    (let ((pos (loop for i from (1- (aref position 0)) downto 0
                                     when (char/= (aref board i (aref position 1)) #\O)
                                       return (list i (aref position 1)) )))
                      pos))
                   ((char= move-char #\v)
                    (let ((pos (loop for i from (1+ (aref position 0)) to (1- my)
                                     when (char/= (aref board i (aref position 1)) #\O)
                                       return (list i (aref position 1)))))
                      pos))))
               (move (move-char pos)
                 (destructuring-bind (y x) pos
                   (cond
                     ((char= move-char #\<)
                      (progn (rotatef
                              (aref board y x) ; #\.
                              (aref board (aref position 0) (1- (aref position 1))) ; #\O
                              (aref board (aref position 0) (aref position 1)) ; #\@
                              )
                             (decf (aref position 1))))
                     ((char= move-char #\>)
                      (progn (rotatef
                              (aref board y x) ; #\.
                              (aref board (aref position 0) (1+ (aref position 1))) ; #\O
                              (aref board (aref position 0) (aref position 1)) ; #\@
                              )
                             (incf (aref position 1))))
                     ((char= move-char #\^)
                      (progn (rotatef
                              (aref board y x) ; #\.
                              (aref board (1- (aref position 0)) (aref position 1)) ; #\O
                              (aref board (aref position 0) (aref position 1)) ; #\@
                              )
                             (decf (aref position 0))))
                     ((char= move-char #\v)
                      (progn (rotatef
                              (aref board y x) ; #\.
                              (aref board (1+ (aref position 0)) (aref position 1)) ; #\O
                              (aref board (aref position 0) (aref position 1)) ; #\@
                              )
                             (incf (aref position 0)))))))
               (try-move (move-char)
                 (let ((pos (find-non-rock move-char)))
                   (when (char= (apply #'aref board pos) #\.)
                     (move move-char pos))))
               (print-board ()
                 (loop for y from 0 to (1- my) do
                   (loop for x from 0 to (1- mx) do
                     (princ (aref board y x)))
                   (princ #\newline))))
        (loop for c across directions do (try-move c))
                                        ;(print-board)
        ))))

(defun score (board)
  (destructuring-bind (my mx) (array-dimensions board)
    (let ((down 0)
          (left 0))
      (loop for y from 0 to (1- my) do
        (loop for x from 0 to (1- mx) do
          (when (char= (aref board y x) #\O)
            (incf down y)
            (incf left x))))
      (+ (* down 100) left))))

(defun part-1 (input-list)
  (multiple-value-bind (board directions) (split-input input-list)
    (simulate board directions)
    (score board)))

;; (part-1 (aoc/read-input-list 15 :small)) ; => 2028 (11 bits, #x7EC)
;; (part-1 (aoc/read-input-list 15 :medium)) ; => 10092 (14 bits, #x276C)
;; (part-1 (aoc/read-input-list 15 :real)) ; => 1526673 (21 bits, #x174B91)


(defun expand-board (board)
  (destructuring-bind (my mx) (array-dimensions board)
    (let* ((wy my)
           (wx (* mx 2))
           (wide-board (make-array (list wy wx))))
      (loop for y from 0 to (1- my) do
        (loop for x from 0 to (1- mx) do
          (cond
            ((char= (aref board y x) #\#) (setf (aref wide-board y (* 2 x)) #\#
                                                (aref wide-board y (1+ (* 2 x))) #\#))
            ((char= (aref board y x) #\O) (setf (aref wide-board y (* 2 x)) #\[
                                                (aref wide-board y (1+ (* 2 x))) #\]))
            ((char= (aref board y x) #\.) (setf (aref wide-board y (* 2 x)) #\.
                                                (aref wide-board y (1+ (* 2 x))) #\.))
            ((char= (aref board y x) #\@) (setf (aref wide-board y (* 2 x)) #\@
                                                (aref wide-board y (1+ (* 2 x))) #\.)))))
      wide-board)))

(defun score-wide (wide-board)
  (destructuring-bind (my mx) (array-dimensions wide-board)
    (let ((down 0)
          (left 0))
      (loop for y from 0 to (1- my) do
        (loop for x from 0 to (1- mx) do
          (when (char= (aref wide-board y x) #\[)
            (incf down y)
            (incf left x))))
      (+ (* down 100) left))))

(defun part-2 (input-list)
  (declare (optimize (debug 3)))
  (multiple-value-bind (board directions) (split-input input-list)
    (let ((wide-board (expand-board board)))
      (destructuring-bind (my mx) (array-dimensions wide-board)
        (let* ((position (find-player wide-board)))
          (labels ((spacep (pos) (char= (apply #'aref wide-board pos) #\.))
                   (try-move-horizontal (move-char)
                     (cond
                       ((char= move-char #\<)
                        (let ((tx (loop for j from (1- (aref position 1)) downto 0
                                        when (not (or (char= (aref wide-board (aref position 0) j) #\[)
                                                      (char= (aref wide-board (aref position 0) j) #\])))
                                          return j)))
                          (when (and (not (null tx)) (char= (aref wide-board (aref position 0) tx) #\.))
                            (loop for x from tx to (1- (aref position 1)) do
                              (rotatef (aref wide-board (aref position 0) x)
                                       (aref wide-board (aref position 0) (1+ x))))
                            (decf (aref position 1)))))
                       ((char= move-char #\>)
                        (let ((tx (loop for j from (1+ (aref position 1)) to (1- mx)
                                        when (not (or (char= (aref wide-board (aref position 0) j) #\[)
                                                      (char= (aref wide-board (aref position 0) j) #\])))
                                          return j)))
                          (when (and (not (null tx)) (char= (aref wide-board (aref position 0) tx) #\.))
                            (loop for x from tx downto (1+ (aref position 1)) do
                              (rotatef (aref wide-board (aref position 0) x)
                                       (aref wide-board (aref position 0) (1- x))))
                            (incf (aref position 1)))))
                       (t (error "find-space-horizontal on bad move char"))))
                   (can-move-vertical (move-char pos)
                     (let* ((ch (apply #'aref wide-board pos))
                            (next-pos (cond ((char= move-char #\^) (list (1- (first pos)) (second pos)))
                                            ((char= move-char #\v) (list (1+ (first pos)) (second pos)))))
                            (next-left-pos
                              (cond
                                ((char= move-char #\^) (list (1- (first pos)) (1- (second pos))))
                                ((char= move-char #\v) (list (1+ (first pos)) (1- (second pos))))))
                            (next-right-pos
                              (cond
                                ((char= move-char #\^) (list (1- (first pos)) (1+ (second pos))))
                                ((char= move-char #\v) (list (1+ (first pos)) (1+ (second pos)))))))
                       (let ((result  (cond
                                        ((char= ch #\#) nil)
                                        ((char= ch #\.) t)
                                        ((char= ch #\[) (or (and (can-move-vertical move-char next-pos)
                                                                 (can-move-vertical move-char next-right-pos))
                                                            (and (spacep next-pos)
                                                                 (spacep next-right-pos))))
                                        ((char= ch #\]) (or (and
                                                             (can-move-vertical move-char next-pos)
                                                             (can-move-vertical move-char next-left-pos))
                                                            (and
                                                             (spacep next-pos)
                                                             (spacep next-left-pos))))
                                        ((char= ch #\@) (or
                                                         (spacep next-pos)
                                                         (can-move-vertical move-char next-pos))))))
                         result)))

                   (try-move-vertical (move-char pos)
                     (when (can-move-vertical move-char pos)
                       (destructuring-bind (y x) pos
                         (let* ((ch (aref wide-board y x))
                                (n (cond ((char= move-char #\^) (list (1- y) x))
                                         ((char= move-char #\v) (list (1+ y) x))))
                                (nl (cond ((char= move-char #\^) (list (1- y) (1- x)))
                                          ((char= move-char #\v) (list (1+ y) (1- x)))))
                                (nr (cond ((char= move-char #\^) (list (1- y) (1+ x)))
                                          ((char= move-char #\v) (list (1+ y) (1+ x))))))
                           (cond
                             ((char= ch #\#) nil)
                             ((char= ch #\.) nil)
                             ((char= ch #\[)
                              (progn
                                (try-move-vertical move-char n)
                                (try-move-vertical move-char nr)
                                (when (and (spacep n) (spacep nr))
                                  (rotatef (aref wide-board (first n) (second n))
                                           (aref wide-board (first pos) (second pos)))
                                  (rotatef (aref wide-board (first n) (1+ (second n)))
                                           (aref wide-board (first pos) (1+ (second pos)))))))
                             ((char= ch #\])
                              (progn
                                (try-move-vertical move-char n)
                                (try-move-vertical move-char nl)
                                (when (and (spacep n) (spacep nl))
                                  (rotatef (aref wide-board (first n) (second n))
                                           (aref wide-board (first pos) (second pos)))
                                  (rotatef (aref wide-board (first n) (1- (second n)))
                                           (aref wide-board (first pos) (1- (second pos)))))))
                             ((char= ch #\@)
                              (progn
                                (try-move-vertical move-char n)
                                (when (spacep n)
                                  (rotatef (aref wide-board (first n) (second n))
                                           (aref wide-board (first pos) (second pos)))
                                  (setf (aref position 0) (first n))
                                  (setf (aref position 1) (second n))))

                              )))))
                     (when (can-move-vertical move-char pos)
                       ))
                   (print-wide-board ()
                     (loop for y from 0 to (1- my) do
                       (loop for x from 0 to (1- mx) do
                         (princ (aref wide-board y x)))
                       (princ #\newline))))
            (loop for c across directions do
              (cond ((char= c #\<) (try-move-horizontal c))
                    ((char= c #\>) (try-move-horizontal c))
                    ((char= c #\^) (try-move-vertical c (coerce position 'list)))
                    ((char= c #\v) (try-move-vertical c (coerce position 'list)))))
            (score-wide wide-board)))))))

;; (part-2 (aoc/read-input-list 15 :medium)) ; => 9021 (14 bits, #x233D)
;; (part-2 (aoc/read-input-list 15 :real)) ; => 1535509 (21 bits, #x176E15)
