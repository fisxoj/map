(defsystem :map
  :serial t
  :description "Octave-like functionality for lisp!"
  :version "0.0.1"
  :author "Matt Novenstern <fisxoj@gmail.com>"
  :components ((:file "package")
	       (:file "conditions")
	       (:module vectors
			:components ((:file "vector")
				     (:file "vector-operations")))
	       (:module matrix
			:components ((:file "matrices")
				     (:file "matrix-predicates")
				     (:file "matrix-operations")
				     (:file "slices")
;				     (:file "matrix-macros")
				     ))
	       (:module calculus
			:components ((:file "integration")
				     (:file "rkf45")))
	       (:module plotting
			:components ((:file "plot")))
	       (:module images
			:components ((:file "image"))))
  :depends-on (#:gsll #:cl-jpeg #:external-program))
