(in-package :ctx.meta)


;; base layers

(defclass base-layer-context (standard-layer-class)
  ())

(defclass ctx-layer-context (standard-layer-class)
  ())


(deflayer ctx-base-layer ()
  ((direct-slot-class :initform 'ctx-direct-slot-definition
		      :accessor direct-slot-class))
  (:metaclass base-layer-context))

(define-layered-method
    adjoin-layer-using-class
    ((layer ctx-layer-context) active-layers)
  ;; on layer activation deactivate other layers of the same layer type
  (call-next-layered-method
   layer
   (remove-layer 'ctx-base-layer active-layers)))
