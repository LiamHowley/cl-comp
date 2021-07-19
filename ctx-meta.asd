(defsystem #:ctx-meta
    :description "Provides a context oriented metaclass."
    :serial t
    :depends-on ("closer-mop"
		 "contextl")
    :components ((:file "package")
		 (:file "ctx-layer")
		 (:file "ctx-meta"))
    :long-description
    #.(read-file-string
       (subpathname *load-pathname* "README.txt"))
    :in-order-to ((test-op (load-op :ctx-utils-test))))
