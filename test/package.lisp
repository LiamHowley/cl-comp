(defpackage stw.meta.test
  (:use :cl
	:parachute
	:stw.meta)
  (:import-from :closer-mop
		:standard-direct-slot-definition
		:slot-definition-name)
  (:export :stw-meta))

(in-package stw.meta.test)

(define-test stw-meta)
