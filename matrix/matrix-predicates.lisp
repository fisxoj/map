;; matrix-predicates.lisp
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

(export '(same-size-p
	  squarep
	  hermitianp))

(defun same-size-p (A B)
  (loop for a in (mapcar #'= (array-dimensions A) (array-dimensions B))
       when (not a) do (return nil)
       finally (return t)))

(defun inner-dimensions-match-p (A B)
  (let ((dimA (array-dimensions A))
	(dimB (array-dimensions B)))
    (and (= (second dimA) (first dimB)))))

(defun squarep (M)
  (apply #'= (array-dimensions M)))

(defun hermitianp (M)
  (and (loop for i from 0 to (1- (array-dimension M 0))
	  collect (loop for j from 0 to (- (array-dimension M 1) i 1)
		     collect (= (conjugate (aref M i j)) (aref M j i))))))

(defun singularp (M)
  (zerop (determinant M)))
