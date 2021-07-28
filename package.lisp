(defpackage stw.meta
  (:use :cl)
  (:import-from :stw.util
		:map-tree-depth-first
		:mappend)
  (:import-from :fare-memoization
		:define-memo-function)
  (:import-from :closer-mop
		:class-finalized-p
		:class-direct-slots
		:class-direct-superclasses
		:validate-superclass
		:standard-class
		:standard-direct-slot-definition
		:direct-slot-definition-class
		:effective-slot-definition-class
		:compute-effective-slot-definition
		:slot-definition-name)
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
  (:export :stw-base-class
	   :base-class

	   ;; slots
	   :direct-slot-class
	   :stw-direct-slot-definition

	   ;;layers
	   :stw-base-layer
	   :stw-layer-context

	   ;;re-exporting from closer-mop
	   :standard-direct-slot-definition

	   ;;re-exporting from contextl
	   :deflayer
	   :partial-class
	   :partial-class-base-initargs
	   :special-layered-access-class
	   :define-layered-class
	   :with-active-layers

	   ;;mop utility functions
	   :filter-slots-by-type
	   :filter-slots-by-layer
	   :find-slot-definition
	   :class-precedents

	   ;;cache tables
	   :*class-precedents*
	   :*filtered-slots*
	   :*slot-definitions*))
