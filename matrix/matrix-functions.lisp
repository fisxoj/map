;; matrix-functions.lisp
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

(defun determinant (matrix)
  (multiple-value-bind (l u p) (lu-decomposition matrix)
    (declare (ignore l))
    ;; L is unitriangular, so (det L) == 1
    (loop
       with u-terms = 1d0
       for i from 0 below (array-dimension matrix 0)
       do (multf u-terms (aref u i i))
       finally (return (* u-terms (expt -1.0 (length p)))))))

(defun inverse (matrix)
;  (declare (type matrix matrix))
  (.* (1/ (determinant matrix))
      (adjugate matrix)))

(defun adjugate (matrix)
  (with-result (result (array-dimensions matrix))
    (do-matrix (result subscripts)
      (setf (apply #'aref result subscripts)
	    ;; -1^(i+j+k+...) power * determinant of the minor matrix
	    (* (expt -1 (reduce #'+ (mapcar #'1+ subscripts)))
	       (determinant (apply #'minor-matrix matrix subscripts)))))))
