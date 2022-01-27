(in-package stw.meta)


;; clos helpers

(declaim (inline class-definition))

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

;; TO BE REWORKED IN PLACES!!!

;;; class precedents

(defvar *class-precedents* (make-hash-table :test #'equal))

(defun class-precedents (class)
  "Find all superclasses in the inheritance hierarchy 
of a class. Results are cached unless nil."
  (let ((class (class-definition class)))
;;    (when (class-finalized-p class)
      (cache-class-precedents class)))

(define-memo-function (cache-class-precedents :table *class-precedents*) (class)
  (class-precedents% class))

(defun class-precedents% (class)
  (let ((parents (class-direct-superclasses class)))
    (append parents
	    (mappend #'map-tree-depth-first #'class-precedents%
		     parents))))


;;; filtered precedents

(defvar *filtered-precedents* (make-hash-table :test #'equal))

(define-memo-function (filter-precedents-by-type :table *filtered-precedents*)
    (class object-type)
  (let ((class (class-definition class)))
    (filtered-precedents class #'(lambda (class)
				   (typep class object-type)))))

(defun filtered-precedents (class filter)
  "Find all superclasses in the inheritance hierarchy of a class"
  (let ((parents (class-direct-superclasses class)))
    (append (remove-if-not filter parents)
	    (mappend #'map-tree-depth-first
		     #'(lambda (class)
			 (filtered-precedents class filter))
		     parents))))


;;; all direct slots, including slots derived from supers.

(defvar *all-slots* (make-hash-table :test #'equal))

(defun all-slots% (class)
  "Returns effective slots as direct slots."
  (append (class-direct-slots class)
	  (mappend #'map-tree-depth-first #'all-slots%
		   (class-direct-superclasses class))))

(define-memo-function (all-slots :table *all-slots*) (class)
  (all-slots% class))


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
  "Returns the direct/effective slot definitions of all slots 
of object-type associated with a class, including 
inherited slots. Recursively walks through superclasses. 
Results are cached unless nil."
  (let ((class (class-definition class)))
    (cache-filter-slots-by-type class object-type)))
  

(defun filtered-effective-slots (class filter)
  (loop for slot in (class-slots class)
     for target-slotp = (funcall filter slot)
     if target-slotp
     collect slot))


(define-memo-function (cache-filter-slots-by-type :table *filtered-slots*) (class object-type)
  (let* ((object-type (if (and (symbolp object-type)
			       (find-class 'object-type nil))
			  (find-class 'object-type)
			  object-type))
	 (predicate #'(lambda (slot)
			(typep slot object-type)))
	 (slots (when (class-finalized-p (class-definition class))
		  (filtered-effective-slots (class-definition class) predicate))))
    (if slots slots
	(filtered-slots (class-definition class) predicate))))



;;; specific slot definition.

(defvar *slot-definitions* (make-hash-table :test #'equal))

(defun find-slot-definition (class slot-name &optional (type 'standard-direct-slot-definition))
  "Return and cache slot definition of slot-name and type.
Results are cached unless nil."
  (let ((class (class-definition class)))
;;    (when (class-finalized-p class)
      (cache-find-slot-definition class slot-name type)))

(define-memo-function (cache-find-slot-definition :table *slot-definitions*) (class slot-name type)
  (let ((slot (when (class-finalized-p (class-definition class))
		(setf slot (loop for slot in (class-slots (class-definition class))
			      when (and (eq (slot-definition-name slot) slot-name)
					(typep slot type))
			      return slot)))))
    (if slot slot
	(loop for slot in (filter-slots-by-type (class-definition class) type)
	   when (eq (slot-definition-name slot) slot-name)
	   return slot))))


;;; defining context for use with special slots

(defmacro define-class-context (env class-type active-layers (&rest rest &key &allow-other-keys)
				  &body body)
  "Sets and captures the dynamic environment for specific contexts. The symbol set with the 
ENV variable is returned along with the captured environment. This encapsulates active layers 
FORM-LAYER and dynamic values of class slot accessors, set through keyword args. 
To be used in conjunction with the WITH-OBJECT-IN-CONTEXT macro."
  (let ((slot (gensym))
	(value (gensym)))
    `(symbol-macrolet ((class-definition ,(find-class class-type)))
       (macrolet ((get-value (,slot ,value)
		    `(cond (,,value ,,value)
			   ((slot-value ,class-definition ',,slot)
			    (slot-value ,class-definition ',,slot))
			   (t nil))))
	 (progn
	   (defdynamic ,env
	     (dynamic-let ((class-type ',class-type))
	       (with-active-layers ,active-layers
		 (dletf (,@(loop for (key value) on rest by #'cddr
			      for slot = (find-symbol (symbol-name key))
			      collect `((,slot class-definition) (get-value ,slot ,value))))
		   ,@body
		   (capture-dynamic-environment))))))))))


(defmacro with-class-in-context (env class-instance &body body)
  "Facilitates access to the dynamic context encapsulated by ENV. 
ENV is set by the DEFINE-CONTEXT macro."
  `(with-dynamic-environment ((dynamic ,env))
     (unless (typep ,class-instance (dynamic class-type))
       (error "the object ~a is not of type ~a. Out of context." ,class-instance (dynamic class-type)))
     (let ((the-class ,class-instance))
       ,@body)))



(defmacro define-layer-context (env active-layers bindings &body body)
  "Sets and captures the dynamic environment for specific contexts and bindings. The symbol set with the 
ENV variable is returned along with the captured environment."
  `(defdynamic ,env
     (with-active-layers ,active-layers
       (dlet ,bindings
	 ,@body
	 (capture-dynamic-environment)))))


(defmacro with-context (env &body body)
  "Facilitates access to the dynamic context encapsulated by ENV. 
ENV is set by the DEFINE-LAYERED-CONTEXT macro."
  `(with-dynamic-environment ((dynamic ,env))
     ,@body))



;; shallow copy

(defun clone-object (original)
  (let ((clone (object-to-plist original :recurse nil)))
    (apply #'make-instance (car clone) (cdr clone))))


(defun object-to-plist (object &key filter (recurse t) (package *package*) use-placeholders) 
  "Recursively walks through a class creating a plist from the initargs
and values of it's slots. The structural model of the data is replicated in
the resulting tree."
  (let* ((class (class-of object))
	 (list (if filter
		   (filter-slots-by-type class filter)
		   (all-slots class)))
	 (keys))
    (labels ((walk (slots acc)
	       (cond ((null slots)
		      acc)
		     ((consp slots)
		      (walk (car slots) (walk (cdr slots) acc)))
		     ((atom slots)
		      (if (typep slots (or filter 'c2mop:standard-direct-slot-definition))
			  (let* ((slot slots)
				 (slot-name (slot-definition-name slot)))
			    (when (slot-boundp object slot-name)
			      (let* ((initarg (car (slot-definition-initargs slot)))
				     (value (when (find-symbol (symbol-name slot-name) package)
					      (unless (member initarg keys)
						(slot-value object slot-name)))))
				(push initarg keys)
				(cond ((and recurse
					    (consp value))
				       (cons initarg (cons (walk value nil) acc)))
				      (value
				       (cons initarg
					     (cons (if use-placeholders
						       initarg
						       value)
						   acc)))
				      (t acc)))))
			  (typecase (class-of slots)
			    (built-in-class
			     (cons (if use-placeholders
				       (car keys)
				       slots)
				   acc))
			    (standard-class
			     (cons (if recurse
				       (object-to-plist slots
							:filter filter
							:use-placeholders use-placeholders
							:package package)
				       slots)
				   acc))
			    (t acc)))))))
      (let ((list (walk list nil)))
	(push (class-name class) list)
	list))))
