(defpackage cl-comp.test
  (:use :cl
	:parachute
	:cl-comp)
  (:import-from :closer-mop
		:standard-direct-slot-definition
		:slot-definition-name)
  (:export :cl-comp))

(in-package cl-comp.test)

(define-test cl-comp)
