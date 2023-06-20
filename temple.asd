(require :asdf)
(defsystem "temple"
  :description "HTML templating library."
  :pathname "src"
  :license "MIT"
  :serial t
  :components
  ((:file "temple"))
  :in-order-to ((test-op (test-op "temple/tests"))))

(defsystem "temple/tests"
  :description "Tests for the temple HTML templating library."
  :pathname "src"
  :depends-on ("temple" "fiveam")
  :components ((:file "temple-tests"))
  :perform (test-op (op system)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :temple :temple/tests))))
