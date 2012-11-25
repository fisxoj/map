(in-package #:map)

(defmacro do-matrix ((matrix &optional subscripts) &body body)
;  (assert (or (not subscripts) (= (length subscripts) (array-rank matrix))))
  (let ((i (gensym)))
    `(loop for ,i from 0 upto (1- (array-total-size ,matrix))
	  for ,subscripts = (row-major-subscripts ,matrix ,i)
	do (progn ,@body))))

(defmacro as-vector (matrix)
  `(make-array (array-total-size ,matrix)
	       :element-type (array-element-type ,matrix)
	       :displaced-to ,matrix
	       :displaced-index-offset 0))
