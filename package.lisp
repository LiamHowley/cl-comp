(defpackage stw.meta
  (:use :cl)
  (:import-from :stw.util
		:ensure-list
		:map-tree-depth-first
		:mappend)
  (:import-from :fare-memoization
		:define-memo-function)
  (:import-from :closer-mop
		:class-finalized-p
		:class-direct-slots
		:class-direct-superclasses
		:class-slots
		:validate-superclass
		:standard-class
		:standard-direct-slot-definition
		:direct-slot-definition-class
		:effective-slot-definition-class
		:compute-effective-slot-definition
		:slot-definition-name
		:slot-definition-initargs
		:slot-value-using-class)
  (:import-from :contextl
		:adjoin-layer-using-class
		:call-next-layered-method
		:class-layer
		:define-layered-class
		:define-layered-function
		:define-layered-method
		:deflayer
		:find-layer-class
		:layer-active-p
		:layered-class
		:layered-effective-slot-definition-in-layers
		:layered-effective-slot-definition
		:partial-class
		:partial-class-base-initargs
		:remove-layer
		:special-layered-access-class
		:special-layered-direct-slot-definition
		:special-layered-effective-slot-definition
		:standard-layer-class

		;; build contextual environment
		:capture-dynamic-environment
		:defdynamic
		:dynamic-let
		:dynamic
		:dletf
		:with-active-layers
		:with-dynamic-environment)
  (:export :stw-base-class
	   :define-base-class
	   :base-class

	   ;; slots
	   :slot-definition-class
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
	   :find-slot-definition
	   :class-precedents
	   :filter-precedents-by-type
	   :all-slots
	   :clone-object
	   :object-to-plist

	   ;;cache tables
	   :*class-precedents*
	   :*filtered-slots*
	   :*slot-definitions*
	   :*all-slots*

	   ;;layered-context
	   :define-class-context
	   :with-class-in-context
	   :define-layer-context
	   :with-context

	   ;; printing
	   :print-layered-object
	   :print-layered-object-p
	   :print-layered-slot))
