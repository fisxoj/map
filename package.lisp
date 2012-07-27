(defpackage #:map
  (:use #:cl)
  (:export
   ;; Matrix math operation
   #:.+
   #:.*

   ;; Matrix predicates
   #:same-size-p
   #:hermitianp
   #:squarep

   ;; Matrix operations
   #:tensor-product
   #:eye
   #:zeros
   #:czeros
   #:mtrace

   ;; Calculus
   #:riemann

   ;; Images
   #:imread

   ;; Plotting
   #:plot
   #:plot3d
   #:image
   ))

(in-package :map)
