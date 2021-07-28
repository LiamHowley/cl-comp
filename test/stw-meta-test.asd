(defsystem #:stw-meta-test
    :description "Test suite for stw-meta."
    :depends-on ("stw-meta" "parachute")
    :serial t
    :components ((:file "package")
		 (:file "stw-meta"))
    :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :stw.meta.test)))
