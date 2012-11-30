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
  (let* ((submatrix-instructions
	  (%submatrix-instructions (array-dimensions matrix) subscripts))
	 (submatrix-dimensions
	  (mapcar #'second submatrix-instructions))
	 (start-points (mapcar #'first submatrix-instructions)))
    ;; If submatrix-dimensions is nil, the array has one element and we should just
    ;; pass things along to regular old aref.
    (if (not (every (lambda (a) (= 1 a)) submatrix-dimensions))
	(reshape-matrix
	 (with-result (submatrix submatrix-dimensions)
	   (do-matrix (submatrix indices)
	     (setf (apply #'aref submatrix indices)
		   (apply #'aref matrix (mapcar #'+ start-points indices))))))
	(apply #'aref matrix subscripts))))

;; FIXME: Implement.
(defun (setf mref) (source-matrix target-matrix &rest subscripts)
  (let* ((submatrix-instructions
	  (%submatrix-instructions (array-dimensions target-matrix) subscripts))
	 (start-points (mapcar #'first submatrix-instructions))
	 (reduced-dimensions
	  (remove-if (lambda (a) (= a 1)) (mapcar #'second submatrix-instructions))))
    (assert (reduce (lambda (a b) (and a b))
		    (mapcar #'= reduced-dimensions (array-dimensions source-matrix))))
    (loop
       for i from 0 upto (1- (array-total-size source-matrix))
       do (setf (apply #'aref target-matrix
		       (mapcar #'+ start-points (%row-major-subscript (mapcar #'second submatrix-instructions) i)))
		(row-major-aref source-matrix i))
       finally (return target-matrix))))

(defun %submatrix-instructions (matrix-dimensions subscripts)
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
       (%submatrix-instructions (cdr matrix-dimensions) rest)))))

(defun reduced-dimensions (matrix)
  (remove-if (lambda (a) (= a 1)) (array-dimensions matrix)))

(defun reshape-matrix (matrix)
  "Utility for the mref functions which takes a submatrix, which sometimes contains arrays with dimensions that are 1.  i.e. a 2x4x1 matrix, which will be far more useful as a 2x4 matrix.  This function makes a displaced array to the original, but with the more useful shape."
  (make-array (reduced-dimensions matrix)
	      :element-type (array-element-type matrix)
	      :displaced-to matrix
	      :displaced-index-offset 0))
