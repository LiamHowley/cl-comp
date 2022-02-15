(in-package :stw.meta)


;;; base layers

(defclass stw-layer-context (standard-layer-class)
  ())

(deflayer stw-base-layer ()
  ()
  (:metaclass stw-layer-context))
