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

(defun zeros (&rest dimensions)
  (make-array dimensions :element-type 'double-float :initial-element 0.0d0))

(defun ones (&rest dimensions)
  (make-array dimensions :element-type 'double-float :initial-element 1.0d0))

(defun complex-zeros (&rest dimensions)
  (make-array dimensions :element-type '(complex double-float) :initial-element #c(0.0d0 0.0d0)))

(defun eye (dimension)
  (loop with m = (zeros dimension dimension)
     for i from 0 to (1- dimension)
     do (setf (aref m i i) 1.0d0)
     finally (return m)))

(defmacro make-plane (range1 range2 function &optional (element-type ''double-float))
  (let ((result (gensym))
	(l1 (gensym))
	(l2 (gensym))
	(i (gensym))
	(j (gensym)))
    `(let* ((,l1 (range-length ,range1))
	    (,l2 (range-length ,range2))
	    (,result (make-array (list ,l1 ,l2) :element-type ,element-type)))
       (declare (optimize speed))
       (loop for ,j of-type fixnum from 0 below ,l2
	    do (loop for ,i of-type fixnum from 0 below ,l1
		    do (setf (aref ,result ,i ,j)
			     (funcall ,function
				      (+ (range-start ,range1) (* ,i (range-delta ,range1)))
				      (+ (range-start ,range2) (* ,j (range-delta ,range2))))))
	    finally (return ,result)))))

(declaim (inline row-major-subscripts))

(defun row-major-subscripts (matrix-dimensions row-major-index)
  (declare (optimize speed))
  (loop
     with p
     for i of-type fixnum in (reverse matrix-dimensions)
     for rmi of-type fixnum = row-major-index then m
     for (m r) (fixnum fixnum) = (multiple-value-list (floor rmi i))
     do (push r p)
     finally (return p)))

(defun subscripts-row-major (matrix &rest subscripts)
  (let ((array-dimensions (array-dimensions matrix))
	(subscripts subscripts))
    (%subscripts-row-major (cdr array-dimensions) subscripts)))

(defun %subscripts-row-major (dimensions subscripts)
  (+ (* (car subscripts) (reduce #'* dimensions))
     (if (cdr dimensions)
	 (%subscripts-row-major (cdr dimensions) (cdr subscripts))
	 (second subscripts))))
