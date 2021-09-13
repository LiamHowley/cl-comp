(in-package :stw.meta)


(defclass base-class
  (special-layered-access-class)
   ())

(defmethod validate-superclass
    ((class base-class)
     (superclass standard-class))
  t)


;; compute definition

(defclass stw-direct-slot-definition (special-layered-direct-slot-definition)
  ())

(defclass stw-effective-slot-definition (special-layered-effective-slot-definition)
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
	     (find-class 'stw-effective-slot-definition))))
      (call-next-method))))


(define-layered-class stw-base-class
 :in-layer stw-base-layer (partial-class base-class) ()
  (:default-initargs :defining-metaclass 'base-class))



;;; helper macro to define a base class

(defmacro define-base-class (&whole form name &body parts)
  (let* ((layer-p (member (car parts) '(:in-layer :in) :test #'eq))
	 (layer (if layer-p
		    (cadr parts)
		    t))
	 (rest (cond (layer-p
		       (cddr parts))
		      ((not (listp (car parts)))
		       (error "illegal option ~s in ~s."
			      (car parts) form))
		      (t parts)))
	 (supers (car rest))
	 (slots (cadr rest))
	 (class-slots (cddr rest)))
    `(define-layered-class ,name
       :in ,layer ,supers ,slots
       ,@class-slots
       ,@(unless (assoc :metaclass class-slots)
	   `((:metaclass stw-base-class))))))
