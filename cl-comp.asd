(defsystem #:cl-comp
    :description "CL-COMP thin wrapper around ContextL, enabling context oriented metaclasses."
    :serial t
    :depends-on ("stw-utils"
		 "closer-mop"
		 "contextl"
		 "fare-memoization")
    :components ((:file "package")
		 (:file "comp-layer")
		 (:file "comp-util")
		 (:file "comp-meta")
		 (:file "print"))
    :long-description
    #.(uiop:read-file-string
       (uiop:subpathname *load-pathname* "docs/README.org"))
    :in-order-to ((test-op (load-op :comp-meta-test))))
