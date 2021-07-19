(in-package ctx.meta)


;; clos helpers

(defun class-definition (class)
  (typecase class
    (symbol (find-class class))
    (string (find-class (intern (string-upcase class))))
    (otherwise class)))

(defvar *class-precedents* (make-hash-table :test #'equal))

(define-memo-function (class-precedents :table *class-precedents*) (class)
  "Find all superclasses in the inheritance hierarchy of a class. Cached function"
  (class-precedents% (class-definition class)))


(defun class-precedents% (class)
  (let ((parents (class-direct-superclasses class)))
    (append parents
	    (mappend #'map-tree-depth-first #'class-precedents%
		     parents))))

(defun filtered-slots (class filter)
  "Returns effective slots as direct slots, filtering for subtypes of OBJECT-TYPE 
and duplicates. Cached function."
  (let ((slots (remove-if-not filter (class-direct-slots class))))
    (append slots
	    (remove-if #'(lambda (slot)
			   (member (slot-definition-name slot)
				   (mapcar #'slot-definition-name slots)))
		       (mappend #'map-tree-depth-first
				#'(lambda (slot)
				    (filtered-slots slot filter))
				(class-direct-superclasses class))))))



(defvar *filtered-slots* (make-hash-table :test #'equal))

(define-memo-function (filter-slots-by-type :table *filtered-slots*) (class object-type)
  "Returns effective slots as direct slots, filtering for subtypes of OBJECT-TYPE. Cached function"
  (let ((object-type (if (and (symbolp object-type)
			      (find-class 'object-type nil))
			 (find-class 'object-type)
			 object-type)))
    (filtered-slots class #'(lambda (slot)
			      (typep slot object-type)))))



(defvar *slot-definitions* (make-hash-table :test #'equal))

(define-memo-function (find-slot-definition :table *slot-definitions*) (class slot-name &optional (type 'standard-direct-slot-definition))
  (loop for slot in (filter-slots-by-type (class-definition class) type)
     when (eq (slot-definition-name slot) slot-name)
     return slot))
