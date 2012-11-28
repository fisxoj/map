;; matrix-macros.lisp
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

(defmacro do-matrix ((matrix &optional subscripts) &body body)
;  (assert (or (not subscripts) (= (length subscripts) (array-rank matrix))))
  (let ((i (gensym)))
    `(loop for ,i from 0 upto (1- (array-total-size ,matrix))
	  for ,subscripts = (row-major-subscripts ,matrix ,i)
	do (progn ,@body))))

(defmacro as-vector (matrix)
  `(make-array (array-total-size ,matrix)
	       :element-type (array-element-type ,matrix)
	       :displaced-to ,matrix
	       :displaced-index-offset 0))
