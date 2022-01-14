(in-package :stw.meta)


(defmethod print-object ((object stw-base-class) stream)
  (if (layer-active-p 'stw-base-layer) 
      ;; set indentation if any before printing object
      (print-layered-object object nil stream 0)
      (call-next-method)))


(define-layered-function print-layered-object-p (object stream &optional indent)
  (:documentation "Print object? Print from cache? Ignore?"))


(define-layered-function print-layered-object (object constraint stream &optional indent)
  (:documentation "Print layered object to stream. Useful to call from PRINT-OBJECT for example.")

  (:method
      :in stw-base-layer (object constraint stream &optional indent)
      (declare (ignore constraint))
      (print-unreadable-object (object stream :type t :identity t))))


(define-layered-function print-layered-slot (object slot type stream)
  (:documentation "Given object slot-definition and slot-definition-type print a representation to stream."))
