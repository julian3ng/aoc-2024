(defsystem "aoc-2024"
  :description "AOC 2024"
  :author "Julian Eng <julian3ng@protonmail.com>"
  :components ((:file "src/day-1"))
  :in-order-to ((test-op (test-op "aoc-2024/tests"))))

(defsystem "aoc-2024/tests"
  :description "AOC 2024 Tests"
  :author "Julian Eng <julian3ng@protonmail.com>"
  :depends-on ("aoc-2024" "fiveam")
  :components ((:file "tests/main")
               (:file "tests/day-1"))
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                        (find-symbol* :all-tests :tests))))
