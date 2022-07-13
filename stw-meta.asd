(defsystem #:stw-meta
    :description "STW-META is a thin wrapper around ContextL, enabling context oriented metaclasses."
    :serial t
    :depends-on ("stw-utils"
		 "closer-mop"
		 "contextl"
		 "fare-memoization")
    :components ((:file "package")
		 (:file "stw-layer")
		 (:file "stw-util")
		 (:file "stw-meta")
		 (:file "print"))
    :long-description
    #.(uiop:read-file-string
       (uiop:subpathname *load-pathname* "README.txt"))
    :in-order-to ((test-op (load-op :stw-meta-test))))
