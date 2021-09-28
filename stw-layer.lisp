(in-package :stw.meta)


;;; base layers

(defclass stw-layer-context (standard-layer-class)
  ((direct-slot-class
    :initform 'stw-direct-slot-definition
    :accessor direct-slot-class)))

(deflayer stw-base-layer ()
  ()
  (:metaclass stw-layer-context))
