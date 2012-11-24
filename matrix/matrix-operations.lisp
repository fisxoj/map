(in-package #:map)

(declaim (optimize (speed 3)))

(defun mapply (func data)
  (loop
     with result = (cl-utilities:copy-array data)  
     for i from 0 below (array-total-size result)
     do (setf (row-major-aref result i)
	      (funcall func (row-major-aref result i)))
     finally (return result)
       ))

(defun tensor-product (A B)
  (declare (type matrix A B))
  (let* ((dimA (array-dimensions A)) (dimB (array-dimensions B))
	 (x (* (elt dimA 0) (elt dimB 0)))
	 (y (* (elt dimA 1) (elt dimB 1))))
    (loop with M = (zeros x y)
       for i from 0 upto (1- (elt dimA 0))
       do (loop for j from 0 upto (1- (elt dimA 1))
	     do (loop for k from 0 upto (1- (elt dimB 0))
		   do (loop for l from 0 upto (1- (elt dimB 1))
			 for xx = (+ k (* (elt dimB 0) i))
			 for yy = (+ l (* (elt dimB 1) j))
			 do (setf (aref M xx yy) (* (aref A i j) (aref B k l))))))
       finally (return-from tensor-product M))))

(defun mtrace (M)
  (when (squarep M)
    (loop for i from 0 to (1- (array-dimension M 0))
	 sum (aref M i i))))

(defun transpose (M)
  (let ((new (make-array (array-dimensions M))))
    (loop for i from 0 upto (1- (array-dimension M 0))
	 for j from 0 upto (1- (array-dimension M 1))
	 do (setf (aref new j i) (aref M i j))
	 return M)))

(defgeneric .+ (A B)
  (:documentation "Matrix addition function, will also add single numbers elementwise to the matrix."))

(defmethod .+ ((A array) (B array))
  (if (same-size-p A B)
      (let ((result (apply 'zeros (array-dimensions A))))
	(loop for i from 0 upto (1- (array-total-size A))
	   do (setf (row-major-aref result i)
		    (+ (row-major-aref A i)
		       (row-major-aref B i)))
	   finally (return result)))
      (error 'dimension-mismatch :a A :b B)))

(defmethod .+ ((A array) (B number))
  (let ((result (apply 'zeros (array-dimensions A))))
    (loop for i from 0 upto (1- (array-total-size A))
       do (setf (row-major-aref result i)
		(+ (row-major-aref A i)
		   B))
       finally (return result))))

(defmethod .+ ((A number) (B array))
  (.+ B A))

(defmethod .+ (A B)
  (error ".+ Not implelented for types ~a and ~a" (type-of A) (type-of B)))


(defmethod .* ((A simple-array) (B simple-array))
  (if (inner-dimensions-match-p A B)
      (let* ((dimA (array-dimensions A))
	     (dimB (array-dimensions B))
	     (dimresult (list (first dimA) (second dimB)))
	     (result (make-array dimresult)))
	(loop for i from 0 upto (1- (first dimresult))
	   do (loop for j from 0 upto (1- (second dimresult))
		   do (loop for k from 0 upto (1- (second dimA))
			   do (incf (aref result i j)
				    (* (aref A i k) (aref B k j)))))))))

(defmethod .* ((A number) (B array))
  (let* ((dimA (array-dimensions B))
	 (result (make-array dimA)))
    (loop for i from 0 upto (1- (array-total-size B))
       do (setf (row-major-aref result i) (* A (row-major-aref B i))))
    result))

(defmethod .* ((A array) (B number))
  (.* B A))

(defmethod .- (A B)
  (.+ A (.* -1 B)))

(defun norm (matrix)
  (.* (1/ (loop for element across (as-vector matrix)
	     maximize element))
      matrix))

;; FIXME: Make this a method?
(defun determinant (matrix)
;  (declare (type matrix matrix))
  (if (squarep matrix)
      (let ((length (array-dimension matrix 0)))
	(loop for i from 0 upto (- length 2)
	   sum (loop
		  with positive-terms = 1.0d0
		  with negative-terms = 1.0d0
		  for j from 0 upto (1- length)
		  do (multf positive-terms (aref matrix (mod (+ i j) length) j))
		  do (multf negative-terms (aref matrix (mod (+ i (* -1 j) -1) length) j))
;		  do (format t "~a ~a~%" (aref matrix (mod (+ i j) length) j) (aref matrix (mod (+ i (* -1 j) -1) length) j))
		  finally (return (- positive-terms negative-terms)))))
      (error 'matrix-not-square)))
