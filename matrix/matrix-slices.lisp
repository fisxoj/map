;; matrix-slices.lisp
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

(in-package :map)

(defun matrix-row (matrix row)
  (mref matrix row t))

(defun matrix-column (matrix column)
  (mref matrix t column))

(defun mref (matrix &rest subscripts)
  "Functions like (aref), but accepts ranges or T and returns a submatrix of matrix."
  (declare (optimize speed space (safety 0)))
  (let* ((submatrix-instructions
	  (%submatrix-instructions (array-dimensions matrix) subscripts))
	 (submatrix-dimensions
	  (mapcar #'second submatrix-instructions))
	 (start-points (mapcar #'first submatrix-instructions))
	 (reduced-dimensions
	  (reduced-dimensions submatrix-dimensions)))
    (declare (type (cons integer) start-points))
    ;; If submatrix-dimensions is nil, the array has one element and we should just
    ;; pass things along to regular old aref.
    (if reduced-dimensions
	(reshape-matrix
	 (with-result (submatrix submatrix-dimensions (array-element-type matrix))
	   (do-matrix (submatrix indices)
	     (let ()
	       (declare (type (cons integer) indices start-points))
	       (setf (apply #'aref submatrix indices)
		     (apply #'aref matrix (mapcar #'+ (the (cons integer) start-points)
						  (the (cons integer) indices))))))))
	;; We received a set of integer subscripts, access the array normally
	(apply #'aref matrix start-points))))

(defun (setf mref) (source-matrix target-matrix &rest subscripts)
  (declare (optimize speed (safety 0))
	   (inline %submatrix-instructions reduced-dimensions))
  (let* ((submatrix-instructions
	  (%submatrix-instructions (array-dimensions target-matrix) subscripts))
	 (submatrix-dimensions
	  (mapcar #'second submatrix-instructions))
	 (start-points (mapcar #'first submatrix-instructions))
	 (reduced-dimensions
	  (reduced-dimensions submatrix-dimensions)))
    (declare (type (cons fixnum) submatrix-dimensions start-points))

    (if reduced-dimensions
	;; reduced-dimensons will be nil if all dimensions were 1
	(loop
	   with total-size fixnum = (array-total-size source-matrix)
	   for i from 0 below total-size
	   do (setf (apply #'aref target-matrix
			   (mapcar (lambda (a b) (declare (type fixnum a b)) (+ a b))
				   start-points (row-major-subscripts submatrix-dimensions i)))
		    (row-major-aref source-matrix i))
	   finally (return target-matrix))
	;; If we're just setting a single value, pass that off to aref
	(setf (apply #'aref target-matrix start-points) source-matrix))))

(defun %submatrix-instructions (matrix-dimensions subscripts)
  (declare (optimize speed)
	   (type (cons fixnum) matrix-dimensions)
	   (type cons subscripts))
  (assert (= (length matrix-dimensions) (length subscripts)))
  (destructuring-bind (sub . rest) subscripts
    (cons
     (typecase sub
       ;; Ranges should have integer values with the exception of the end
       ;; which can be -1
       (range
	(list
	 (range-start sub)
	 ;; For the end, accept -1 and calculate the remaining length from
	 ;; the start value
	 (cond
	   ((= (the fixnum (range-stop sub)) -1)
	    (- (car matrix-dimensions) (the fixnum (range-start sub))))
	   (t (range-length sub)))))
       (integer
	(list sub 1))
       (t
	(list 0 (car matrix-dimensions))))
     (when rest (%submatrix-instructions (cdr matrix-dimensions) rest)))))

(defun reduced-dimensions (dimensions)
  "Removes 1's from dimension lists.

NOTE: This will return nil if all dimensions are 1"
  (declare (optimize speed))
  (remove-if (lambda (a) (declare (type fixnum a)) (= a 1)) dimensions))

(defun reshape-matrix (matrix)
  (declare (optimize speed))
  "Utility for the mref functions which takes a submatrix, which sometimes contains arrays with dimensions that are 1.  i.e. a 2x4x1 matrix, which will be far more useful as a 2x4 matrix.  This function makes a displaced array to the original, but with the more useful shape."
  (make-array (reduced-dimensions (array-dimensions matrix))
	      :element-type (array-element-type matrix)
	      :displaced-to matrix
	      :displaced-index-offset 0))
