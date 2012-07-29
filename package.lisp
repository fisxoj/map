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

   ;; Vector operations
   #:norm

   ;; Calculus
   #:riemann
   #:rkf45

   ;; Images
   #:imread

   ;; Plotting
   #:plot
   #:plot3d
   #:image
   ))

(in-package :map)
