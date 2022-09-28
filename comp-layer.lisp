(in-package :cl-comp)


;;; base layers

(defclass comp-layer-context (standard-layer-class)
  ())

(deflayer comp-base-layer ()
  ()
  (:metaclass comp-layer-context))


(define-layered-method adjoin-layer-using-class
  ((layer comp-layer-context) active-layers)
  ;; on layer activation deactivate other layers of the same layer type
  (values 
   (call-next-layered-method
    layer
    (remove-layer 'comp-base-layer active-layers))
   t))
