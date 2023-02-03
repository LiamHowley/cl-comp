(in-package cl-comp)


;; clos helpers

(declaim (inline class-definition))

(defun class-definition (class)
  (typecase class
    (symbol (find-class class))
    (string (find-class (intern (string-upcase class))))
    (otherwise class)))


;; TO BE REWORKED IN PLACES!!!

;;; class precedents

(defvar *class-precedents* (make-hash-table :test #'equal))

(defun class-precedents (class &optional update-cache)
  "Find all superclasses in the inheritance hierarchy 
of a class. Results are cached unless nil."
  (let ((class (class-definition class)))
    (when update-cache
      (remhash (list class slot-name type) *class-precedents*))
    (cache-class-precedents class)))

(define-memo-function (cache-class-precedents :table *class-precedents*) (class)
  (class-precedents% class))

(defun class-precedents% (class)
  (let ((parents (class-direct-superclasses class)))
    (append parents
	    (mappend #'map-tree-depth-first #'class-precedents%
		     parents))))


(defvar *class-precedent* (make-hash-table :test #'equal))

(defun find-class-precedent (class precedent type &optional update-cache)
  "Find the precedent (name) in the list of precedents of class, by type.
When present, returns the precedent class."
  (when update-cache
    (remhash (list class precedent type) *class-precedent*))
  (cache-find-class-precedent class precedent type))

(define-memo-function (cache-find-class-precedent :table *class-precedent*) (class precedent type)
  (loop for class in (filter-precedents-by-type (class-definition class) type)
	when (eq (class-name class) precedent)
	  return class))


;;; filtered precedents

(defvar *filtered-precedents* (make-hash-table :test #'equal))

(define-memo-function (filter-precedents-by-type :table *filtered-precedents*)
    (class object-type)
  (map-filtered-precedents (class-definition class)
			   #'(lambda (class)
			       (typep class object-type))))

(defun map-filtered-precedents (class &optional (filter (constantly t)) (map #'identity))
  "Find all superclasses in the inheritance hierarchy of a class, 
filtered by type"
  (let ((parents (class-direct-superclasses class)))
    (append (remove-if-not filter parents)
	    (mapcar map
		    (mappend #'map-tree-depth-first
			     #'(lambda (class)
				 (map-filtered-precedents class filter map))
			     parents)))))


;;; all direct slots, including slots derived from supers.

(defvar *all-slots* (make-hash-table :test #'equal))

(defun all-slots% (class)
  "Returns all direct slot definitions including 
those of class precedents."
  (append (class-direct-slots class)
	  (mappend #'map-tree-depth-first #'all-slots%
		   (class-direct-superclasses class))))

(define-memo-function (all-slots :table *all-slots*) (class)
  (all-slots% class))


;;; filtered slots

(defun map-filtered-slots (class &optional (filter (constantly t)) (map #'identity))
  (let ((slots (remove-if-not filter (class-direct-slots class))))
    (append slots
	    (mapcar map
		    (mappend #'map-tree-depth-first
			     #'(lambda (class)
				 (map-filtered-slots class filter map))
			     (class-direct-superclasses class))))))



(defvar *filtered-slots* (make-hash-table :test #'equal))

(defun filter-slots-by-type (class object-type &optional update-cache)
  "Returns the direct slot definitions of all slots 
of object-type associated with a class, including 
inherited slots. Recursively walks through superclasses. 
Results are cached unless nil."
  (let ((class (class-definition class)))
    (when update-cache
      (remhash (list class object-type) *filtered-slots*))
    (cache-filter-slots-by-type class object-type)))
  

(define-memo-function (cache-filter-slots-by-type :table *filtered-slots*) (class object-type)
  (let* ((object-type (if (and (symbolp object-type)
			       (find-class 'object-type nil))
			  (find-class 'object-type)
			  object-type))
	 (predicate #'(lambda (slot)
			(typep slot object-type))))
    (remove-duplicates (map-filtered-slots class predicate)
		       :from-end t
		       :test #'(lambda (slota slotb)
				 (eq (slot-definition-name slota) (slot-definition-name slotb))))))



;;; specific slot definition.

(defvar *slot-definitions* (make-hash-table :test #'equal))

(defun find-slot-definition (class slot-name type &optional update-cache)
  "Return and cache slot definition of slot-name and type.
Results are cached unless nil."
  (when update-cache
    (remhash (list class slot-name type) *slot-definitions*))
  (cache-find-slot-definition class slot-name type))

(define-memo-function (cache-find-slot-definition :table *slot-definitions*) (class slot-name type)
  (loop for slot in (filter-slots-by-type (class-definition class) type)
	when (eq (slot-definition-name slot) slot-name)
	  return slot))



;;; defining context for use with special slots

(defmacro define-class-context (env class-type active-layers (&rest rest &key &allow-other-keys)
				  &body body)
  "Sets and captures the dynamic environment for specific contexts. 
Both ENV and CLASS-TYPE are symbols with the former dynamically bound within 
the macro body. The symbol set to the ENV variable is bound by the captured 
environment and returned. This captured environment encapsulates active layers 
 and dynamic (special) values of class slot accessors, set through keyword args. 
To be used in conjunction with the WITH-CLASS-IN-CONTEXT macro."
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
  "Facilitates access to the dynamic context bound to ENV. 
ENV is set by the DEFINE-CLASS-CONTEXT macro."
  `(with-dynamic-environment ((dynamic ,env))
     (unless (typep ,class-instance (dynamic class-type))
       (error "the object ~a is not of type ~a. Out of context." ,class-instance (dynamic class-type)))
     (let ((the-class ,class-instance))
       ,@body)))



(defmacro define-layer-context (env active-layers bindings &body body)
  "Sets and captures the dynamic environment for specific contexts and bindings. The symbol set by the 
ENV variable is bound to the captured environment and returned. To be used with the WITH-CONTEXT macro."
  `(defdynamic ,env
     (with-active-layers ,active-layers
       (dlet ,bindings
	 ,@body
	 (capture-dynamic-environment)))))


(defmacro with-context (env &body body)
  "Facilitates access to the dynamic context encapsulated by ENV. 
ENV is set by the DEFINE-LAYERED-CONTEXT macro."
  `(when (typep (dynamic ,env) 'dynamic-environment)
     (progn
       (with-dynamic-environment ((dynamic ,env))
	 ,@body))))


(defmacro delete-context (env)
  "Delete the captured dynamic environment."
  `(setf (dynamic ,env) nil))



;; shallow copy

(defun clone-object (original)
  "Makes shallow copy of clos object."
  (let ((clone (object-to-plist original :recurse nil)))
    (apply #'make-instance (car clone) (cdr clone))))


(defun slots-with-values
    (class &key (type 'standard-direct-slot-definition) (filter-if (constantly nil)) (filter-if-not (constantly t)))
  "Returns the list of slots belonging to class that
are bound to a value. For convenience it also returns
a list of slot names."
  (let ((record nil))
    (loop
      for slot in (filter-slots-by-type (class-of class) type)
      for slot-name = (slot-definition-name slot)
      when (and (slot-boundp class slot-name)
		(slot-value class slot-name)
		(funcall filter-if-not slot))
	unless (or (member slot-name record :test #'eq)
		   (funcall filter-if slot))
	  collect slot-name into slot-names
	  and collect slot into slots
	  and do (push slot-name record)
      finally (return (values slots slot-names)))))


(defun object-to-plist
    (object &key
	      filter (recurse t) use-placeholders with-object-name
	      (map
	       #'(lambda (object slot)
		   (let ((slot-name (slot-definition-name slot)))
		     (when (slot-boundp object slot-name)
		       (slot-value object slot-name))))))
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
		      (nreverse acc))
		     ((consp slots)
		      (if (consp (car slots))
			  (walk (cdr slots) (cons (walk (car slots) nil) acc))
			  (walk (cdr slots) (walk (car slots) acc))))
		     ((typep slots (or filter 'c2mop:standard-direct-slot-definition))
		      (let* ((slot slots)
			     (slot-name (slot-definition-name slot))
			     (initarg (car (slot-definition-initargs slot)))
			     (value (funcall map object slot)))
			(when value
			  (push slot-name keys)
			  (setf acc
				(cond ((and recurse
					    (consp value))
				       (cons (walk value nil) (cons initarg acc)))
				      (value
				       (cons (if use-placeholders
						 slot-name
						 (if (and (class-of value)
							  recurse
							  (typep (class-of value) 'standard-class))
						     ;; When the value of a slot is another
						     ;; user-defined object, and recurse is T.
						     (object-to-plist value
								      :filter filter
								      :use-placeholders use-placeholders
								      :recurse recurse
								      :map map)
						     value))
					     (cons initarg acc)))
				      (t acc))))
			acc))
		     ((and (class-of slots)
			   (typep (class-of slots) 'standard-class))
		      (cons (if recurse
				(object-to-plist slots
						 :filter filter
						 :use-placeholders use-placeholders
						 :recurse recurse
						 :map map)
				slots)
			    acc))
		     ((and (class-of slots)
		    	   (typep (class-of slots) 'built-in-class))
		      (cons (if use-placeholders
		    		(car keys)
		    		slots)
		    	    acc))
		     (t acc))))
      (let ((list (walk list nil)))
	(when with-object-name
	  (push (class-name class) list))
	list))))


(defun equality (ob1 ob2)
  "Functionally if not structurally equal: i.e. it facilitates equality 
testing between user-defined objects of different types."
  (flet ((set-obj (obj)
	   (if (typep (class-of obj) 'built-in-class)
	       obj
	       (object-to-plist obj))))
    (equal (set-obj ob1)(set-obj ob2))))
