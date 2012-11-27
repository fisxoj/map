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
   #:mref
   #:determinant
   #:inverse
   #:row-major-subscripts
   #:subscripts-row-major

   ;; Matrix macros
   #:do-matrix
   #:as-vector

   ;; Vector operations
   #:norm
   #:dot

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
