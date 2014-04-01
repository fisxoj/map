;; matrix-decomposition.lisp
;;
;; Copyright (c) 2012-2013 Matt Novenstern <fisxoj@gmail.com>
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

(defun lu-decomposition (matrix)
  "Performs the LU decomposition of the matrix according to Golub & Van Loan, Matrix Computations, Algorithm 3.4.1, as referenced by the GSL docs."
  ;; See: http://books.google.com/books?id=mlOa7wPX6OYC&lpg=PA114&ots=lcfrg9N7fY&dq=Golub%20%26%20Van%20Loan%2C%20Matrix%20Computations%2C%20Algorithm%203.4.1&pg=PA112#v=onepage&q=Golub%20&%20Van%20Loan,%20Matrix%20Computations,%20Algorithm%203.4.1&f=false
  (declare (optimize speed (safety 0)))
  (let* ((length (array-dimension matrix 0))
	 ;; The permutation vector will hold the
	 (p nil)
	 ;; Copy matrix into U
	 (U (.* 1d0 matrix))
	 (rows (make-instance 'range :stop (1- length) :delta 1)))

    (declare (optimize speed (safety 1) (debug 0))
	     (type (simple-array double-float) U))

    (dotimes (k length)

      (setf (range-start rows) k)
      ;; There seems to be an error in the book where the permutations should happen
      ;; to a whole row, now just k:n columns of it
      (let ((ps (loop
		   with max double-float = 0d0
		   with row integer = 0 
		   for i of-type integer from k below length
		   for v double-float = (aref U i k)

		     when (> v max)
		     do (setf max v) and do (setf row i)
		     finally (return row))))
	(declare (type integer ps))
	;; If the most significant row is the k-th row, don't juggle it with itself
	
	(unless (= k ps)
	  (push ps p)
	  (rotatef (mref U k t) (mref U ps t))))

      (unless (eps= (aref U k k) 0.0)
	;; wow, this is straightforward with (mref), hopefully it's not super slow
	(setf  (range-start rows) (1+ k)
	       (mref U rows k) (./ (mref U rows k) (aref U k k))
	       (mref U rows rows) (.- (mref U rows rows) (.* (mref U rows k) (mref U k rows))))))
    ;; Finish the p-vector

    (values
     ;; Put 1's on the diagonal
     (let ((lower (lower-matrix U)))
       (loop for i from 0 below length
	  do (setf (aref lower i i) 1d0) finally (return lower))) 
     ;; Upper Matrix
     (upper-matrix U)
     (reverse p))))

(defun permutation-list (matrix &key (sort #'>) (column 0) (start 0))
  (declare (optimize (speed 3))
	   (type function sort))
  (mapcar
       #'second
       (sort (loop
		for i from start below (array-dimension matrix 0)
		collect (list (aref matrix i column) i))
	     sort :key (lambda (n) (abs (car n))))))

(defun permutation-matrix (p-list)
  (let ((l (length p-list)))
    (with-result (result (list l l))
      (dotimes (i l)
	(setf (aref result i (elt p-list i)) 1d0)))))
