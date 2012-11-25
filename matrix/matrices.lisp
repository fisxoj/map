(in-package #:map)

(deftype real-matrix () '(array double-float *))
(deftype complex-matrix () '(array (complex double-float) *))
(deftype matrix () '(or real-matrix complex-matrix))

(deftype matrix () '(or real-matrix complex-matrix))

(defun zeros (&rest dimensions)
  (make-array dimensions :element-type 'double-float :initial-element 0.0d0))

(defun ones (&rest dimensions)
  (make-array dimensions :element-type 'double-float :initial-element 1.0d0))

(defun czeros (&rest dimensions)
  (make-array dimensions :element-type '(complex double-float) :initial-element #c(0.0d0 0.0d0)))

(defun eye (dimension)
  (loop with m = (zeros dimension dimension)
     for i from 0 to (1- dimension)
     do (setf (aref m i i) 1.0d0)
     finally (return m)))

(defun row-major-subscripts (matrix row-major-index)
  (%row-major-subscript (array-dimensions matrix) row-major-index))

(defun %row-major-subscript (matrix-dimensions row-major-index)
;  (format t "~a ~a~%" matrix-dimensions row-major-index)  
  (when matrix-dimensions
    (multiple-value-bind (multiple remainder)
	(floor row-major-index (car matrix-dimensions))
      (cons remainder (%row-major-subscript (cdr matrix-dimensions) multiple)))))

