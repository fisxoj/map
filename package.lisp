(defpackage #:map
  (:use #:cl)
  (:export
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

   ;; Matrix operations
   #:tensor-product
   #:eye
   #:zeros
   #:ones
   #:czeros
   #:mtrace

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
