(in-package :stw.meta)


;;; base layers

(defclass stw-layer-context (standard-layer-class)
  ())

(deflayer stw-base-layer ()
  ()
  (:metaclass stw-layer-context))


(define-layered-method adjoin-layer-using-class
  ((layer stw-layer-context) active-layers)
  ;; on layer activation deactivate other layers of the same layer type
  (values 
   (call-next-layered-method
    layer
    (remove-layer 'stw-base-layer active-layers))
   t))
