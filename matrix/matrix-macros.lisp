(in-package #:map)

(defmacro domatrix ((element matrix &optional dimensions) &body body)
  (declare (ignore dimensions))
  (let ((i (gensym)))
    `(loop for ,i from 0 upto (1- (array-total-size ,matrix))
	  for ,element = (row-major-aref ,matrix ,i)
	;; FIXME: Bind a list of indices to this array if a variable is provided
	;; when ,indices
	;; do (setf ,indices (indices-from-row-major ,matrix ,i))
	  
	do (progn ,@body))))
