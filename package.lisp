(defpackage ctx.meta
  (:use :cl)
  (:import-from :closer-mop
		:validate-superclass
		:standard-class
		:direct-slot-definition-class
		:effective-slot-definition-class
		:compute-effective-slot-definition)
  (:import-from :contextl
		:deflayer
		:standard-layer-class
		:adjoin-layer-using-class
		:define-layered-class
		:define-layered-method
		:call-next-layered-method
		:remove-layer
		:partial-class
		:partial-class-base-initargs
		:special-layered-access-class
		:special-layered-direct-slot-definition
		:special-layered-effective-slot-definition
		:find-layer
		:class-layer)
  (:export :ctx-base-class
	   :base-class

	   ;; slots
	   :direct-slot-class
	   :ctx-direct-slot-definition

	   ;;layers
	   :ctx-base-layer
	   :ctx-layer-context

	   ;;re-exporting from contextl
	   :deflayer))
