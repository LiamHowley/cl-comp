(defsystem #:ctx-meta
    :description "Provides a context oriented metaclass."
    :serial t
    :depends-on ("ctx-utils"
		 "closer-mop"
		 "contextl"
		 "fare-memoization")
    :components ((:file "package")
		 (:file "ctx-layer")
		 (:file "ctx-meta")
		 (:file "ctx-util"))
    :long-description
    #.(read-file-string
       (subpathname *load-pathname* "README.txt"))
    :in-order-to ((test-op (load-op :ctx-meta-test))))
