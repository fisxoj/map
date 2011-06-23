(defpackage :map
  (:use :cl)
  (:export
   ;; Matrix math operation
   #:.+
   ;; Matrix predicates
   #:same-size-p
   ;; Matrix operations
   #:tensor-product))

(in-package :map)

(defsystem map
  :description "Octave-like functionality for lisp!"
  :version "0.0.1"
  :author "Matt Novenstern <fisxoj@gmail.com>"
  :components ((:file "matrices")
	       (:file "vector")
	       (:file "matrix-predicates"
		      :depends-on ("matrices"))
	       (:file "matrix-math"
		      :depends-on ("matrices"))
	       (:file "matrix-operations"
		      :depends-on ("matrices"))
	       (:file "matrix-macros"
		      :depends-on ("matrices"))))

