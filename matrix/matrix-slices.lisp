(in-package :map)

(defun matrix-row (matrix row)
  (mref matrix row t))

(defun matrix-column (matrix column)
  (mref matrix t column))

(defun mref (matrix &rest subscripts)
  "Functions like (aref), but accepts ranges or T and returns a submatrix of matrix."
  (let* ((submatrix-instructions
	  (%submatrix-dimensions (array-dimensions matrix) subscripts))
	 (submatrix-dimensions
	  (mapcar #'second submatrix-instructions))
	 (submatrix (apply #'zeros submatrix-dimensions))
	 (start-points (mapcar #'first submatrix-instructions)))
    ;; If submatrix-dimensions is nil, the array has one element and we should just
    ;; pass things along to regular old aref.

    (if submatrix-dimensions
	(reshape-matrix
	 (loop
	    for i from 0 upto (1- (array-total-size submatrix))
	    do (setf (row-major-aref submatrix i)
		     (apply #'aref matrix (mapcar #'+ start-points (row-major-subscripts submatrix i))))
	    finally (return submatrix)))
	(apply #'aref matrix subscripts))))

;; FIXME: Implement.
(defun (setf mref) (matrix &rest subscripts)
  )

(defun %submatrix-dimensions (matrix-dimensions subscripts)
  (assert (= (length matrix-dimensions) (length subscripts)))
  (when subscripts
    (destructuring-bind (sub . rest) subscripts
      (cons
       (cond
	 ;; Ranges should have integer values with the exception of the end
	 ;; which can be -1
	 ((rangep sub)
	  (list
	   (range-start sub)
	   ;; For the end, accept -1 and calculate the remaining length from
	   ;; the start value
	   (cond
	     ((= (range-stop sub) -1) (- (car matrix-dimensions) (range-start sub)))
	     (t (range-length sub)))))
	 ((integerp sub)
	  (list sub 1))
	 ((eq sub t)
	  (list 0 (car matrix-dimensions))))
       (%submatrix-dimensions (cdr matrix-dimensions) rest)))))

(defun reduced-dimensions (matrix)
  (remove-if (lambda (a) (= a 1)) (array-dimensions matrix)))

(defun reshape-matrix (matrix)
  "Utility for the mref functions which takes a submatrix, which sometimes contains arrays with dimensions that are 1.  i.e. a 2x4x1 matrix, which will be far more useful as a 2x4 matrix.  This function makes a displaced array to the original, but with the more useful shape."
  (make-array (reduced-dimensions matrix)
	      :element-type (array-element-type matrix)
	      :displaced-to matrix
	      :displaced-index-offset 0))
