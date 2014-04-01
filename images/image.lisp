;; image.lisp
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

(in-package #:map)

(defun imread (pathname)
  (multiple-value-bind  (image width height color-channels) (jpeg:decode-image pathname)
    (make-array (list width height color-channels)
		:displaced-to image :displaced-index-offset 0)))

(defun discrete-cosine-transform (m)
  "Implements the DCT-II algorithm."
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type matrix m))
  (map:with-result (result (array-dimensions m) 'double-float)
    (let* ((width (array-dimension m 0))
	   (height (array-dimension m 1))
	   (pi-over-2width (/ pi (* 2 width)))
	   (pi-over-2height (/ pi (* 2 height)))
	   (sum 0))
      (dotimes (k width)
	(dotimes (l height)
	  (setf (aref result k l)
		(*
		 1/4
		 (if (zerop k)
		     (/ 1 (sqrt 2))
		     1)
		 (if (zerop l)
		     (/ 1 (sqrt 2))
		     1)
		 (loop
		   for j of-type fixnum from 0 below height
		   summing (loop
			     for i of-type fixnum from 0 below width
			     summing (* (aref m i j)
					(cos (* (1+ (* 2 i)) k pi-over-2width))
					(cos (* (1+ (* 2 j)) l pi-over-2height))))))))))))

(defun inverse-discrete-cosine-transform (m)
  "Implements the DCT-II algorithm."
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type matrix m))
  (map:with-result (result (array-dimensions m) 'double-float)
    (let* ((width (array-dimension m 0))
	   (height (array-dimension m 1))
	   (pi-over-2width (/ pi (* 2 width)))
	   (pi-over-2height (/ pi (* 2 height))))
      (map:do-matrix (result (k l))
	(setf (aref result k l)
	      (/
	       (loop
		 for j from 1 below height
		 summing (loop
			   for i from 1 below width
			   summing (* (if (zerop k)
					  (/ 1 (sqrt 2))
					  1)
				      (if (zerop l)
					  (/ 1 (sqrt 2))
					  1)
				      (aref m i j)
				      (cos (* (1+ (* 2 i)) k pi-over-2width))
				      (cos (* (1+ (* 2 j)) l pi-over-2height)))))
	       4))))))
