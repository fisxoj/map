(defpackage #:map
  (:use #:cl)
  (:export
   ;; Missing functions
   #:multf
   #:1/

   ;; Matrix type definitions
   #:complex-matrix
   #:real-matrix
   #:matrix

   ;; Matrix math operation
   #:.+
   #:.*

   ;; Matrix predicates
   #:same-size-p
   #:hermitianp
   #:squarep
   #:singularp

   ;; Matrix operations
   #:tensor-product
   #:norm
   #:eye
   #:zeros
   #:ones
   #:czeros
   #:mtrace
   #:mapply
   #:determinant
   #:inverse
   #:row-major-subscripts

   ;; Matrix macros
   #:domatrix
   #:as-vector

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

   ;; Printing utility functions
   #:mute
   ))

(in-package :map)
