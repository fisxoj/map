;; matrix-multiply.lisp
;;
;; Copyright (c) 2013 Matt Novenstern <fisxoj@gmail.com>
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

(defun %*rr (aa bb)
  "Real by real matrix multiplication"
  (declare (optimize speed (safety 0) (debug 1)))
  (let* ((rows (array-dimension aa 0))
	 (cols (array-dimension bb 1))
	 (inner (array-dimension aa 1))
	 (rr (map:zeros rows cols)))
    (declare (type (simple-array double-float (* *)) aa bb rr)
	     (type fixnum rows cols inner))
    (dotimes (i rows)
      (let ((i-rows (the fixnum (* i rows))))
	(declare (type fixnum i-rows))
	(dotimes (k inner)
	  (let ((a-value (row-major-aref aa (the fixnum (+ i-rows k))))
		(k-inner (the fixnum (* k inner))))
	    (declare (type double-float a-value)
		     (type fixnum k-inner))
	    (unless (zerop a-value)
	      (dotimes (j cols)
		(incf (row-major-aref rr (+ i-rows j)) (* a-value (row-major-aref bb (+ k-inner j))))))))))
    rr))

(defun %*cr (aa bb)
  "Real by real matrix multiplication"
  (declare (optimize speed (safety 0) (debug 1)))
  (let* ((rows (array-dimension aa 0))
	 (cols (array-dimension bb 1))
	 (inner (array-dimension aa 1))
	 (rr (map:zeros rows cols)))
    (declare (type (simple-array double-float (* *)) bb)
	     (type (simple-array (complex double-float) (* *)) aa rr)
	     (type fixnum rows cols inner))
    (dotimes (i rows)
      (let ((i-rows (the fixnum (* i rows))))
	(declare (type fixnum i-rows))
	(dotimes (k inner)
	  (let ((a-value (row-major-aref aa (the fixnum (+ i-rows k))))
		(k-inner (the fixnum (* k inner))))
	    (declare (type (complex double-float) a-value)
		     (type fixnum k-inner))
	    (unless (= a-value #c(0d0 0d0))
	      (dotimes (j cols)
		(incf (row-major-aref rr (+ i-rows j)) (* a-value (row-major-aref bb (+ k-inner j))))))))))
    rr))

(defun %*rc (aa bb)
  "Real by complex matrix multiplication"
  (declare (optimize speed (safety 0) (debug 1)))
  (let* ((rows (array-dimension aa 0))
	 (cols (array-dimension bb 1))
	 (inner (array-dimension aa 1))
	 (rr (map:zeros rows cols)))
    (declare (type (simple-array double-float (* *)) aa)
	     (type (simple-array (complex double-float) (* *)) bb rr)
	     (type fixnum rows cols inner))
    (dotimes (i rows)
      (let ((i-rows (the fixnum (* i rows))))
	(declare (type fixnum i-rows))
	(dotimes (k inner)
	  (let ((a-value (row-major-aref aa (the fixnum (+ i-rows k))))
		(k-inner (the fixnum (* k inner))))
	    (declare (type double-float a-value)
		     (type fixnum k-inner))
	    (unless (zerop a-value)
	      (dotimes (j cols)
		(incf (row-major-aref rr (+ i-rows j)) (* a-value (row-major-aref bb (+ k-inner j))))))))))))

(defun %*cc (aa bb)
  "Complex by complex matrix multiplication"
  (declare (optimize speed (safety 0) (debug 1)))
  (let* ((rows (array-dimension aa 0))
	 (cols (array-dimension bb 1))
	 (inner (array-dimension aa 1))
	 (rr (map:zeros rows cols)))
    (declare (type (simple-array (complex double-float) (* *)) aa bb rr)
	     (type fixnum rows cols inner))
    (dotimes (i rows)
      (let ((i-rows (the fixnum (* i rows))))
	(declare (type fixnum i-rows))
	(dotimes (k inner)
	  (let ((a-value (row-major-aref aa (the fixnum (+ i-rows k))))
		(k-inner (the fixnum (* k inner))))
	    (declare (type (complex double-float) a-value)
		     (type fixnum k-inner))
	    (unless (zerop a-value)
	      (dotimes (j cols)
		(incf (row-major-aref rr (+ i-rows j)) (* a-value (row-major-aref bb (+ k-inner j))))))))))))

