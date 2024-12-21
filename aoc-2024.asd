(defsystem "aoc-2024"
  :description "AOC 2024"
  :author "Julian Eng <julian3ng@protonmail.com>"
  :depends-on ("april" "cl-ppcre")
  :components ((:file "src/queue")
               (:file "src/utils")
               (:file "src/day-1")
               (:file "src/day-2")
               (:file "src/day-3")
               (:file "src/day-4")
               (:file "src/day-5")
               (:file "src/day-6")
               (:file "src/day-7")
               (:file "src/day-8")
               (:file "src/day-9")
               (:file "src/day-10")
               (:file "src/day-11")
               (:file "src/day-12")
               (:file "src/day-13")
               (:file "src/day-14")
               (:file "src/day-15")
               (:file "src/day-16")
               (:file "src/day-17")
               (:file "src/day-18")
               (:file "src/day-19")
               (:file "src/day-20"))
  :in-order-to ((test-op (test-op "aoc-2024/tests"))))

(defsystem "aoc-2024/tests"
  :description "AOC 2024 Tests"
  :author "Julian Eng <julian3ng@protonmail.com>"
  :depends-on ("aoc-2024" "fiveam" "april")
  :components ((:file "src/utils")
               (:file "tests/main")
               (:file "tests/day-1")
               (:file "tests/day-2")
               (:file "tests/day-3")
               (:file "tests/day-4")
               (:file "tests/day-5")
               (:file "tests/day-6")
               (:file "tests/day-8"))
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                        (find-symbol* :all-tests :tests))))
