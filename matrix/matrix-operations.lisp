(in-package #:map)

(declaim (optimize (speed 3)))

(export '(tensor-product
	  mtrace))

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

(defmethod .+ ((A simple-array) (B simple-array))
  (if (same-size-p A B)
      (let ((result (apply 'zeros (array-dimensions A))))
	(loop for i from 0 upto (1- (array-total-size A))
	   do (setf (row-major-aref result i)
		    (+ (row-major-aref A i)
		       (row-major-aref B i)))
	   finally (return result)))
      (error 'dimension-mismatch :a A :b B)))

(defmethod .+ ((A simple-array) (B number))
  (let ((result (apply 'zeros (array-dimensions A))))
    (loop for i from 0 upto (1- (array-total-size A))
       do (setf (row-major-aref result i)
		(+ (row-major-aref A i)
		   B))
       finally (return result))))

(defmethod .+ ((A number) (B simple-array))
  (.+ B A))


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

(defmethod .* ((B array) (A number))
  (.* B A))
