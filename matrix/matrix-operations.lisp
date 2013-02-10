;; matrix-operations.lisp
;;
;; Copyright (c) 2012 Matt Novenstern <fisxoj@gmail.com>
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 3 of
;; the GNU Affero General Public License as published by
;; the Free Software Foundation.
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose. See the GNU
;; Affero General Public License for more details.
;;
;; Version 3 of the GNU Affero General Public License is in the file
;; LICENSE that was distributed with this file.
;; If it is not present, you can access it from
;; https://www.gnu.org/licenses/agpl.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA
;;

(in-package #:map)

(declaim (optimize (speed 3)))

(defun minor-matrix (matrix &rest subscripts)
  ;; Result matrix is of dimensions N-1xN-1 compared to the original matrix
  (let ((row-major 0))
    (do-matrix-with-result (matrix indices result (mapcar #'1- (array-dimensions matrix)) (array-element-type matrix))
      ;; Don't copy a number if any of the indices matches a sprcified subscript
      (unless (reduce (lambda (a b) (or a b)) (mapcar #'= indices subscripts))
	(setf (row-major-aref result row-major) (apply #'aref matrix indices))
	(incf row-major)))))

(defun random-matrix (&rest dimensions)
  (do-matrix-with-result (result subscripts result dimensions)
    (setf (apply #'aref result subscripts) (random 1.0d0))))

(defun mapply (func data)
  (with-result (result (array-dimensions data))
    (loop
       for i from 0 below (array-total-size result)
       do (setf (row-major-aref result i)
		(funcall func (row-major-aref result i))))))

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

(defun transpose (matrix)
  (do-matrix-with-result (result (i j) result (nreverse (array-dimensions matrix)))
    (setf (aref result i j) (aref matrix j i))))

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

(defmethod .+ ((A number) (B number))
  (+ A B))

(defmethod .+ (A B)
  (error ".+ Not implelented for types ~a and ~a" (type-of A) (type-of B)))

(defgeneric .* (A B))

(defmethod .* ((A number) (B number))
  (declare (optimize speed)
	   (type double-float A B))
  (* A B))

(defmethod .* ((A simple-array) (B simple-array))
  (declare (optimize speed)
	   (type (simple-array double-float (* *)) A B))
  (do-matrix-with-result (result (i j) result (list (array-dimension A 0) (array-dimension B 1)))
    (setf (aref result i j) (dot (mref a i t) (mref b t j)))))

(defmethod .* ((A array) (B array))
  (declare (optimize speed)
	   (type (vector double-float) A)
	   (type (array double-float (* *)) B))
  (do-matrix-with-result (result (i j) result (list (array-dimension A 0) (array-dimension B 1)))
    (setf (aref result i j) (dot (mref a i t) (mref b t j)))))

(defmethod .* ((A vector) (B vector))
  (declare (optimize speed)
	   (type (vector double-float *) A B))
  (do-matrix-with-result (result (i j) result (list (length A) (length B)))
    (setf (aref result i j) (* (aref A i) (aref B j)))))

(defmethod .* ((A simple-array) (B vector))
    (declare (optimize speed)
	   (type (simple-array double-float (* *)) A)
	   (type (vector double-float) B))
    (with-result (result (array-dimensions B))
      (loop for i from 0 below (length B)
	 do (setf (aref result i) (.* B (mref A i t))))))

(defmethod .* ((A vector) (B simple-array))
  (declare (optimize speed)
	   (type (vector double-float) A)
	   (type (simple-array double-float (* *)) B))
  (with-result (result (list (length A)))
    (loop for i from 0 below (1- (length A))
       do (setf (aref result i) (.* A (mref B i t))))))

(defmethod .* ((A number) (B array))
  (with-result (result (array-dimensions B))
    (loop for i from 0 below (array-total-size B)
       do (setf (row-major-aref result i) (* A (row-major-aref B i))))
    result))

(defmethod .* ((A number) (B simple-array))
  (declare (optimize speed)
	   (type double-float A)
	   (type (simple-array double-float) B))
  (with-result (result (array-dimensions B))
    (loop for i from 0 below (array-total-size B)
       do (setf (row-major-aref result i) (* A (row-major-aref B i))))
    result))

(defmethod .* ((A number) (B array))
  (declare (optimize speed)
	   (type double-float A)
	   (type (array double-float) B))
  (with-result (result (array-dimensions B))
    (loop for i from 0 below (array-total-size B)
       do (setf (row-major-aref result i) (* A (row-major-aref B i))))
    result))

(defmethod .* ((A array) (B number))
  (.* B A))

(defmethod .* ((A number) (B vector))
  (declare (optimize speed)
	   (type double-float A)
	   (type (vector double-float) B))
  (do-matrix-with-result (B (i) result (list (length B)))
    (setf (aref result i) (* A (aref B i)))))

(defmethod .* ((A number) (B simple-vector))
  (declare (optimize speed)
	   (type double-float A)
	   (type simple-vector B))
  (do-matrix-with-result (B (i) result (list (length B)))
    (setf (aref result i) (* A (aref B i)))))

(defmethod .* ((A vector) (B number))
  (.* B A))

(defgeneric .- (A B))

(defmethod .- (A B)
  (.+ A (.* -1d0 B)))

(defmethod norm ((matrix array))
  (.* (1/ (loop for element across (as-vector matrix)
	     maximize element))
      matrix))

(defgeneric .^ (A B))

(defmethod .^ ((A array) (p integer))
  (assert (squarep A))
  (cond
    ((zerop p) (eye (array-dimension A 0)))
    ((= 1 p) A)
    ((> p 1) (.* A (.^ A (1- p))))
    (t (error ".^ Not implemented for non-positive-integer powers"))))

(defgeneric ./ (A B))

(defmethod ./ ((A simple-array) (B number))
  (do-matrix-with-result (result subs result (array-dimensions A))
    (setf (apply #'aref result subs) (/ (apply #'aref A subs) B))))

(defmethod ./ ((A vector) (B number))
  (do-matrix-with-result (A (i) result (list (length A)))
    (setf (aref result i) (/ (aref A i) B))))

(defmethod ./ ((A number) (B number))
  (/ A B))

(defun upper-matrix (matrix)
  (with-result (result (array-dimensions matrix) (array-element-type matrix))
    (loop
       for i from 0 below (array-dimension matrix 1) by 1
       with range = #r(0 -1)
       do (setf (range-start range) i
		(mref result i range ) (mref matrix i range)))))

(defun lower-matrix (matrix)
  (with-result (result (array-dimensions matrix) (array-element-type matrix))
    (loop
       for i from 0 below (array-dimension matrix 1) by 1
       with range = #r(0 -1)
       do (setf (range-start range) i
		(mref result range i) (mref matrix range i)))))

