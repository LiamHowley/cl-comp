(in-package :ctx.meta)


(define-layered-class base-class
    (special-layered-access-class)
  ())

(defmethod validate-superclass
    ((class base-class)
     (superclass standard-class))
  t)


;; compute definition

(defclass ctx-direct-slot-definition (special-layered-direct-slot-definition attribute-class)
  ())

(defclass ctx-effective-slot-definition (special-layered-effective-slot-definition)
  ())


(defun slot-definition-class (class)
  (handler-case (direct-slot-class (find-layer (class-layer class)))
    (error () nil)))


(defmethod direct-slot-definition-class
    ((class base-class) &key &allow-other-keys)
  (let ((slot-definition-class (slot-definition-class class)))
    (if slot-definition-class
	slot-definition-class
	(call-next-method))))

(defvar *effective-slot-definition*)

(defmethod effective-slot-definition-class
    ((class base-class) &key &allow-other-keys)
  (if *effective-slot-definition*
      *effective-slot-definition*
      (call-next-method)))


(defmethod compute-effective-slot-definition ((class base-class) name direct-slot-definitions)
  (declare (ignore name))
  (flet ((attributep (slot) (typep slot (slot-definition-class class)))) 
    (let ((*effective-slot-definition*
	   (when (find-if #'attributep direct-slot-definitions)
	     (find-class 'ctx-effective-slot-definition))))
      (call-next-method))))


(defclass ctx-base-class
    (partial-class base-class) ()
  (:default-initargs :defining-metaclass 'base-class))
