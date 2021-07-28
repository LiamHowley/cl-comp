(in-package :stw.meta)


;; base layers

(defclass stw-layer-context (standard-layer-class)
  ())

(deflayer stw-base-layer ()
  ((direct-slot-class :initform 'stw-direct-slot-definition
		      :accessor direct-slot-class))
  (:metaclass stw-layer-context))
