(in-package stw.meta.test)

;;layers setup
(deflayer test-layer-1 (stw-base-layer)
  ((direct-slot-class :initform 'test-1-direct-slot-definition))
  (:metaclass stw-layer-context))

(deflayer test-layer-2 (stw-base-layer)
  ((direct-slot-class :initform 'test-2-direct-slot-definition))
  (:metaclass stw-layer-context))


;; referenced slot definition classes

(defclass test-1-direct-slot-definition (stw-direct-slot-definition)
  ((test-slot :initform t :accessor test-1-slot-p)))

(defclass test-2-direct-slot-definition (stw-direct-slot-definition)
  ((test-slot :initform t :accessor test-2-slot-p)))


;; setup class definition slot

(defclass class-definition-slots ()
  ((class-definition-slot
    :initarg :class-definition-slot
    :reader definition-slot-p)))


(defmethod partial-class-base-initargs append ((class class-definition-slots))
  '(:class-definition-slot))

;; metaclass inheritors alongside partial-class
(defclass test-base-class-1
  (base-class)
 ())

(defclass test-base-class-2
  (base-class class-definition-slots)
 ())

(define-layered-class stw-base-class
 :in-layer test-layer-1 (partial-class test-base-class-1) ())

(define-layered-class stw-base-class
 :in-layer test-layer-2 (partial-class test-base-class-2) ())


(defmethod shared-initialize :after ((class test-base-class-2) slot-names &key)
  (declare (ignore slot-names))
  (with-slots (class-definition-slot) class
    (print class)
    (setf class-definition-slot "something interesting")))

(define-layered-class test-class
    :in test-layer-1 ()
    ((slot-a :initarg :slot-a :reader slot-a))
    (:metaclass stw-base-class))

(define-layered-class test-class
    :in test-layer-2 ()
    ((slot-a :initarg :slot-a :reader slot-a))
    (:metaclass stw-base-class))


(defparameter instance (make-instance 'test-class :slot-a t))


(define-test sanity
    :parent stw-meta
    ;; sanity check 1
    (of-type stw-base-class (class-of instance))
    ;; sanity check 2
    (true (slot-value instance 'slot-a)))


(defparameter slot1 (find-slot-definition (find-class 'test-class) 'slot-a 'test-1-direct-slot-definition))

(define-test  metaclass-slots-layer-1
  :parent stw-meta
  :depends-on (sanity)
  (of-type test-1-direct-slot-definition slot1)
  (is eq (slot-definition-name slot1) 'slot-a)
  (true (test-1-slot-p slot1)))

(defparameter slot2 (find-slot-definition (find-class 'test-class) 'slot-a 'test-2-direct-slot-definition))

(define-test  metaclass-slots-layer-2
  :parent stw-meta
  :depends-on (sanity)
  (of-type test-2-direct-slot-definition slot2)
  (is eq (slot-definition-name slot2) 'slot-a)
  (true (test-2-slot-p slot2)))

(define-test class-slot-test
  :parent stw-meta
  :depends-on (sanity)
  (is equal (definition-slot-p (find-class 'test-class)) "something interesting"))
