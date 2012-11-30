(defsystem :map
  :serial t
  :description "Octave-like functionality for lisp!"
  :version "0.1.0"
  :author "Matt Novenstern <fisxoj@gmail.com>"
  :components ((:file "package")
	       (:file "conditions")
	       (:module utilities
			:components ((:file "printing")
				     (:file "missing-functions")
				     (:file "convenience")))
	       (:module vectors
			:components ((:file "vector")
				     (:file "range")
				     (:file "vector-operations")))
	       (:module matrix
			:components ((:file "matrices")
				     (:file "matrix-macros")
				     (:file "matrix-predicates")
				     (:file "matrix-operations")
				     (:file "matrix-slices")
				     ))
	       (:module calculus
			:components ((:file "integration")
				     (:file "rkf45")))
	       (:module plotting
			:components ((:file "plot")))
	       (:module images
			:components ((:file "image"))))
  :depends-on (#:cl-jpeg #:external-program))
