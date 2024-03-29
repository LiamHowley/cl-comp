(in-package cl-comp.test)

;;;; setting up

(eval-when (:compile-toplevel :load-toplevel :execute)

  ;;layers setup

  (defclass test-layer-1-metaclass (comp-layer-context)
    ())

  (defclass test-layer-2-metaclass (comp-layer-context)
    ())

  (deflayer test-layer-1 (comp-base-layer)
    ()
    (:metaclass test-layer-1-metaclass))

  (deflayer test-layer-2 (comp-base-layer)
    ()
    (:metaclass test-layer-2-metaclass))


  ;; referenced slot definition classes

  (defclass test-1-direct-slot-definition (comp-direct-slot-definition)
    ((test-slot :initform t :accessor test-1-slot-p)))

  (defclass test-2-direct-slot-definition (comp-direct-slot-definition)
    ((test-slot :initform t :accessor test-2-slot-p)))


  (defmethod slot-definition-class ((class test-layer-1-metaclass))
    'test-1-direct-slot-definition)

  (defmethod slot-definition-class ((class test-layer-2-metaclass))
    'test-2-direct-slot-definition)


  ;; setup class definition slot

  (define-layered-class
      class-definition-slots ()
    ((class-definition-slot
      :initarg :class-definition-slot
      :special t
      :reader definition-slot-p)))


  (defmethod partial-class-base-initargs
      append ((class class-definition-slots))
      '(:class-definition-slot))

  ;; metaclass inheritors alongside partial-class
  (defclass test-base-class-1
      (base-class)
    ())

  (defclass test-base-class-2
      (base-class class-definition-slots)
    ())

  (define-layered-class comp-base-class
    :in-layer test-layer-1 (partial-class test-base-class-1) ())

  (define-layered-class comp-base-class
    :in-layer test-layer-2 (partial-class test-base-class-2) ())


  (defmethod shared-initialize
      :after ((class test-base-class-2) slot-names &key)
      (declare (ignore slot-names))
      (with-slots (class-definition-slot) class
	(setf class-definition-slot "something interesting")))

  (define-base-class test-class
    :in test-layer-1 ()
    ((slot-a :initarg :slot-a :reader slot-a)))

  (define-base-class test-class
    :in test-layer-2 ()
    ((slot-a :initarg :slot-a :reader slot-a))))


(defparameter instance (make-instance 'test-class :slot-a t))

(define-class-context context1
    test-class
    (test-layer-2)
    (:definition-slot-p "something else"))


;;;;; tests

(define-test sanity
  :parent cl-comp
  ;; sanity check 1
  (of-type comp-base-class (class-of instance))
  (of-type test-base-class-1 (class-of instance))
  (of-type test-base-class-2 (class-of instance))
  ;; sanity check 2
  (true (slot-value instance 'slot-a)))


(defparameter slot1 (find-slot-definition (find-class 'test-class) 'slot-a 'test-1-direct-slot-definition))

(define-test metaclass-slots-layer-1
  :parent cl-comp
  :depends-on (sanity)
  (of-type test-1-direct-slot-definition slot1)
  (is eq (slot-definition-name slot1) 'slot-a)
  (true (test-1-slot-p slot1)))

(defparameter slot2 (find-slot-definition (find-class 'test-class) 'slot-a 'test-2-direct-slot-definition))

(define-test metaclass-slots-layer-2
  :parent cl-comp
  :depends-on (sanity)
  (of-type test-2-direct-slot-definition slot2)
  (is eq (slot-definition-name slot2) 'slot-a)
  (true (test-2-slot-p slot2)))

(define-test class-slot-test
  :parent cl-comp
  :depends-on (sanity)
  (is equal (definition-slot-p (find-class 'test-class)) "something interesting")
  (with-context
      context1
      (is equal (definition-slot-p (find-class 'test-class)) "something else"))
  (with-class-in-context
      context1 instance
    (is equal (definition-slot-p (class-of instance)) "something else"))
  (is equal (definition-slot-p (find-class 'test-class)) "something interesting"))

(define-test copy-object
  :parent cl-comp
  :depends-on (sanity)
  (let ((cloned-obj (clone-object instance)))
    (of-type comp-base-class (class-of cloned-obj))
    (of-type test-class cloned-obj)
    (is eq (slot-a cloned-obj) t)))



(defclass occupation ()
  ((job-type :initarg :job-type)
   (qualification :initarg :qualification)))

(defclass complex-person ()
  ((name :initarg :name)
   (email :initarg :email)
   (interests :initarg :interests)
   (occupations :initarg :occupations)))


(defparameter *complex-person* (make-instance 'complex-person
					      :name "Michele"
					      :email "michele@testemail.com"
					      :interests '("reading" "soccer" "taekwondo")
					      :occupations (list (make-instance 'occupation
										:job-type "accountant"
										:qualification "professional")
								 (make-instance 'occupation
										:job-type "actuary"
										:qualification "professional"))))

(define-test object->plist
  :parent cl-comp
  :depends-on (sanity)
  (is equal `(test-class :slot-a t :slot-a t)
      (object-to-plist instance :with-object-name t))
  (is equal `(test-class :slot-a t)
      (object-to-plist instance :with-object-name t :filter 'test-1-direct-slot-definition))
  (is equal
      `(complex-person
	:name "Michele"
	:email "michele@testemail.com"
	:interests ("reading" "soccer" "taekwondo")
	:occupations ((occupation :job-type "accountant" :qualification "professional")
		      (occupation :job-type "actuary" :qualification "professional")))
      (object-to-plist *complex-person* :with-object-name t))
  (is equal
      `(complex-person
	:name name
	:email email 
	:interests (interests interests interests)
	:occupations ((occupation :job-type job-type :qualification qualification)
		      (occupation :job-type job-type :qualification qualification)))
      (object-to-plist *complex-person* :use-placeholders t :with-object-name t)))
