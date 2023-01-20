(defpackage cl-comp
  (:use :cl)
  (:import-from
   :stw.util
   :ensure-list
   :map-tree-depth-first
   :mappend)
  (:import-from :fare-memoization
   :define-memo-function)
  (:import-from
   :closer-mop
   :finalize-inheritance
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
  (:import-from
   :contextl
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
   :singleton-class
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
   :dlet
   :dletf
   :with-active-layers
   :dynamic-environment
   :with-dynamic-environment)
  (:export
   :comp-base-class
   :define-base-class
   :base-class

   ;; initializing
   :initialize-in-context

   ;; slots
   :slot-definition-class
   :comp-direct-slot-definition

   ;;layers
   :comp-base-layer
   :comp-layer-context

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
   :map-filtered-slots
   :filter-slots-by-type
   :find-slot-definition
   :class-precedents
   :map-filtered-precedents
   :filter-precedents-by-type
   :find-class-precedent
   :all-slots
   :clone-object
   :object-to-plist
   :slots-with-values

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
   :delete-context

   ;; printing
   :print-layered-object
   :print-layered-object-p
   :print-layered-slot

   ;; singleton / helper
   :serialize
   :serialized-p))
