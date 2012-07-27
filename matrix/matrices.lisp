(in-package #:map)

(deftype real-matrix () '(simple-array double-float *))
(deftype complex-matrix () '(simple-array (complex double-float) *))

(deftype matrix () '(or real-matrix complex-matrix))

(defun zeros (&rest dimensions)
  (make-array dimensions :element-type 'double-float :initial-element 0.0d0))

(defun czeros (&rest dimensions)
  (make-array dimensions :element-type '(complex double-float) :initial-element #c(0.0d0 0.0d0)))

(defun eye (dimension)
  (loop with m = (zeros dimension dimension)
     for i from 0 to (1- dimension)
     do (setf (aref m i i) 1.0d0)
     finally (return m)))
