(in-package :cl-comp)


(defclass base-class
  (special-layered-access-class)
   ())

(defmethod validate-superclass
    ((class base-class)
     (superclass standard-class))
  t)

(defmethod validate-superclass
    ((superclass standard-class)
     (class base-class))
  t)

;; compute definition

(defclass comp-direct-slot-definition (special-layered-direct-slot-definition)
  ())

(defclass comp-effective-slot-definition (special-layered-effective-slot-definition)
  ())

(defgeneric slot-definition-class (class)
  (:method (layer) nil))

(defmethod direct-slot-definition-class
    ((class base-class) &key &allow-other-keys)
  (let* ((layer-class (find-layer-class (class-layer class)))
	 (slot-definition-class (slot-definition-class layer-class)))
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
	     (find-class 'comp-effective-slot-definition))))
      (call-next-method))))



(define-layered-function initialize-in-context (class &rest rest &key &allow-other-keys)
  (:documentation "Called from the body of :around qualified shared-initialize, after
the main method has executed. Facilitates layer specific operations on the layered metaclass.
Note: due to the indirection inherent in the use of a layered class structure, this may be 
invoked more than once for each layer.")

  (:method ((class base-class) &key &allow-other-keys)
    class)

  (:method ((slot comp-direct-slot-definition) &key &allow-other-keys)
    slot))


(defmethod shared-initialize :around ((class base-class) slot-names &rest rest &key &allow-other-keys)
  (declare (ignore slot-names))
  (call-next-method)
  (apply #'initialize-in-context class rest))

(defmethod shared-initialize :around ((slot comp-direct-slot-definition) slot-names &rest rest &key &allow-other-keys)
  (declare (ignore slot-names))
  (call-next-method)
  (apply #'initialize-in-context slot rest))


(define-layered-class comp-base-class
 :in-layer comp-base-layer (partial-class)()
  (:default-initargs :defining-metaclass 'base-class))

(defclass serialize ()()
  (:metaclass singleton-class))


;;; helper macros to define a base class

;; I'm not too happy with applying the WITH-ACTIVE-LAYERS macro
;; here. It would be infinitely preferable to have the layer
;; activation enforced via mop/contextl machinery. If DEFINE-BASE-CLASS is
;; not used but the metaclass is specified directly and outside of
;; the appropriately activated context, it won't work as intended.
;; Indeed the wrong layer being activated could entail a trip to
;; the wrong initialize-in-context method altogether or a method-not-found
;; error.

(defmacro define-base-class (&whole form name &body parts)
  (macrolet ((set-attr (attribute &optional value)
	       `(unless (member ,attribute slot)
		  (setf slot (append slot `(,,attribute ,,value))))))
    (let* ((layer-p (member (car parts) '(:in-layer :in) :test #'eq))
	   (layer (if layer-p
		      (cadr parts)
		      t))
	   (parts (cond (layer-p
			 (cddr parts))
			((not (listp (car parts)))
			 (error "illegal option ~s in ~s."
				(car parts) form))
			(t parts)))
	   (supers (car parts))
	   (rest (cdr parts))
	   (instance-slots (car rest))
	   (class-slots (cdr rest))
	   (slots (mapcar #'(lambda (slot)
			      (setf slot (ensure-list slot))
			      (set-attr :initarg (intern (string-upcase (symbol-name (car slot))) 'keyword))
			      slot)
			  instance-slots)))
      `(with-active-layers (,layer)
	 (define-layered-class ,name
	   :in ,layer ,supers ,slots
	   ,@class-slots
	   ,@(unless (assoc :metaclass class-slots)
	       `((:metaclass comp-base-class))))))))


(defun serialized-p (supers)
  (and supers
       (loop for class in supers
	       thereis (filter-precedents-by-type class 'singleton-class))))
