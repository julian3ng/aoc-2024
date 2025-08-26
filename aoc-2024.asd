(defsystem "aoc-2024"
  :description "AOC 2024"
  :author "Julian Eng <julian3ng@protonmail.com>"
  :depends-on ("april" "cl-ppcre")
  :components (
               (:file "src/day-1" :depends-on ("src/utils"))
               (:file "src/day-2" :depends-on ("src/utils"))
               (:file "src/day-3" :depends-on ("src/utils"))
               (:file "src/day-4" :depends-on ("src/utils"))
               (:file "src/day-5" :depends-on ("src/utils"))
               (:file "src/day-6" :depends-on ("src/utils"))
               (:file "src/day-7" :depends-on ("src/utils"))
               (:file "src/day-8" :depends-on ("src/utils"))
               (:file "src/day-9" :depends-on ("src/utils"))
               (:file "src/day-10" :depends-on ("src/utils"))
               (:file "src/day-11" :depends-on ("src/utils"))
               (:file "src/day-12" :depends-on ("src/utils"))
               (:file "src/day-13" :depends-on ("src/utils"))
               (:file "src/day-14" :depends-on ("src/utils"))
               (:file "src/day-15" :depends-on ("src/utils"))
               (:file "src/day-16" :depends-on ("src/utils"))
               (:file "src/day-17" :depends-on ("src/utils"))
               (:file "src/day-18" :depends-on ("src/utils" "src/queue"))
               (:file "src/day-19" :depends-on ("src/utils"))
               (:file "src/day-20" :depends-on ("src/utils"))
               (:file "src/day-21" :depends-on ("src/utils" "src/queue"))
               (:file "src/day-22" :depends-on ("src/utils"))
               (:file "src/day-23" :depends-on ("src/utils"))
               (:file "src/day-24" :depends-on ("src/utils"))
               (:file "src/day-25" :depends-on ("src/utils"))
               (:file "src/queue")
               (:file "src/utils"))
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
