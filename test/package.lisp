(defpackage stw.meta.test
  (:use :cl
	:parachute
	:stw.meta)
  (:import-from :closer-mop
		:standard-direct-slot-definition
		:slot-definition-name))

(in-package stw.meta.test)

(define-test stw-meta)
