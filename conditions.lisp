(in-package #:map)

(define-condition dimension-mismatch ()
  ((A :reader dimension-mismatch-a :initarg :a)
   (B :reader dimension-mismatch-b :initarg :b))
  (:report (lambda (condition stream)
	     (format stream "Matrix A ~a must be the same size as B ~a"
		     (array-dimensions (dimension-mismatch-a condition))
		     (array-dimensions (dimension-mismatch-b condition))))))
