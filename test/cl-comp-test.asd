(defsystem #:cl-comp-test
    :description "Test suite for stw-meta."
    :depends-on ("cl-comp" "parachute")
    :serial t
    :components ((:file "package")
		 (:file "comp-meta"))
    :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :cl-comp.test)))
