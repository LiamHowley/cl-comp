(defsystem #:stw-meta
    :description "Provides a context oriented metaclass."
    :serial t
    :depends-on ("stw-utils"
		 "closer-mop"
		 "contextl"
		 "fare-memoization")
    :components ((:file "package")
		 (:file "stw-layer")
		 (:file "stw-util")
		 (:file "stw-meta"))
    :long-description
    #.(uiop:read-file-string
       (uiop:subpathname *load-pathname* "README.txt"))
    :in-order-to ((test-op (load-op :stw-meta-test))))
