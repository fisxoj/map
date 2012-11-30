;; matrices.lisp
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

(deftype real-matrix () '(array double-float *))
(deftype complex-matrix () '(array (complex double-float) *))
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

(defun subscripts-row-major (matrix &rest subscripts)
  (let ((array-dimensions (array-dimensions matrix))
	(subscripts subscripts))
    (%subscripts-row-major (cdr array-dimensions) subscripts)))

(defun %subscripts-row-major (dimensions subscripts)
  (+ (* (car subscripts) (reduce #'* dimensions))
     (if (cdr dimensions)
	 (%subscripts-row-major (cdr dimensions) (cdr subscripts))
	 (second subscripts))))

(defun minor-matrix (matrix &rest subscripts)
  (let ((result (apply #'zeros (mapcar #'1- (array-dimensions matrix))))
	(row-major 0))
    (do-matrix (matrix indices)
      ;; Don't copy a number if any of the indices matches a sprcified subscript
      (unless (reduce (lambda (a b) (or a b)) (mapcar #'= indices subscripts))
	(setf (row-major-aref result row-major) (apply #'aref matrix indices))
	(incf row-major)))
    result))

(defun random-matrix (&rest dimensions)
  (with-result (result dimensions)
    (do-matrix (result subscripts)
      (setf (apply #'aref result subscripts) (random 1.0d0)))))
