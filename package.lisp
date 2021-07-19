(defpackage ctx.meta
  (:use :cl)
  (:import-from :ctx.util
		:map-tree-depth-first
		:mappend)
  (:import-from :fare-memoization
		:define-memo-function)
  (:import-from :closer-mop
		:validate-superclass
		:standard-class
		:standard-direct-slot-definition
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

	   ;;re-exporting from closer-mop
	   :standard-direct-slot-definition

	   ;;re-exporting from contextl
	   :deflayer
	   :partial-class-base-initargs

	   ;;mop utility functions
	   :filter-slots-by-type
	   :filter-slots-by-layer
	   :find-slot-definition
	   :class-precedents

	   ;;cache tables
	   :*class-precedents*
	   :*filtered-slots*
	   :*slot-definitions*))
