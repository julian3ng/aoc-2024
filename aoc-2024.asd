(defsystem "aoc-2024"
  :description "AOC 2024"
  :author "Julian Eng <julian3ng@protonmail.com>"
  :depends-on ("april")
  :components ((:file "src/utils")
               (:file "src/day-1")
               (:file "src/day-2"))
  :in-order-to ((test-op (test-op "aoc-2024/tests"))))

(defsystem "aoc-2024/tests"
  :description "AOC 2024 Tests"
  :author "Julian Eng <julian3ng@protonmail.com>"
  :depends-on ("aoc-2024" "fiveam" "april")
  :components ((:file "src/utils")
               (:file "tests/main")
               (:file "tests/day-1")
               (:file "tests/day-2"))
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                        (find-symbol* :all-tests :tests))))
