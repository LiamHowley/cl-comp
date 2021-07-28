(in-package stw.meta)


;; clos helpers

(defun class-definition (class)
  (typecase class
    (symbol (find-class class))
    (string (find-class (intern (string-upcase class))))
    (otherwise class)))


;; In all cached function calls below, whether
;; calling class-direct-slots or class-direct-superclasses,
;; a class must be finalized before a result will be returned.
;; I.e. only cache when the class is finalized.
;; Otherwise return nil.

;; Cache tables are exported in package.lisp, so that they can
;; be cleared when class definitions are redefined.

;;; class precedents

(defvar *class-precedents* (make-hash-table :test #'equal))

(defun class-precedents (class)
  "Find all superclasses in the inheritance hierarchy 
of a class. Results are cached unless nil."
  (let ((class (class-definition class)))
    (when (class-finalized-p class)
      (cache-class-precedents class))))

(define-memo-function (cache-class-precedents :table *class-precedents*) (class)
  (class-precedents% class))

(defun class-precedents% (class)
  (let ((parents (class-direct-superclasses class)))
    (append parents
	    (mappend #'map-tree-depth-first #'class-precedents%
		     parents))))


;;; filtered slots

(defun filtered-slots (class filter)
  (let ((slots (remove-if-not filter (class-direct-slots class))))
    (append slots
	    (remove-if #'(lambda (slot)
			   (member (slot-definition-name slot)
				   (mapcar #'slot-definition-name slots)))
		       (mappend #'map-tree-depth-first
				#'(lambda (class)
				    (filtered-slots class filter))
				(class-direct-superclasses class))))))



(defvar *filtered-slots* (make-hash-table :test #'equal))

(defun filter-slots-by-type (class object-type)
  "Returns the direct slot definitions of all slots 
of object-type associated with a class, including 
inherited slots. Recursively walks through superclasses. 
Results are cached unless nil."
  (let ((class (class-definition class)))
    (when (class-finalized-p class)
      (cache-filter-slots-by-type class object-type))))
  

(define-memo-function (cache-filter-slots-by-type :table *filtered-slots*) (class object-type)
  (let ((object-type (if (and (symbolp object-type)
			      (find-class 'object-type nil))
			 (find-class 'object-type)
			 object-type)))
    (filtered-slots class #'(lambda (slot)
			      (typep slot object-type)))))



;;; specific slot definition.

(defvar *slot-definitions* (make-hash-table :test #'equal))

(defun find-slot-definition (class slot-name &optional (type 'standard-direct-slot-definition))
  "Return and cache slot definition of slot-name and type.
Results are cached unless nil."
  (let ((class (class-definition class)))
    (when (class-finalized-p class)
      (cache-find-slot-definition class slot-name type))))

(define-memo-function (cache-find-slot-definition :table *slot-definitions*) (class slot-name type)
  (loop for slot in (filter-slots-by-type (class-definition class) type)
     when (eq (slot-definition-name slot) slot-name)
     return slot))
