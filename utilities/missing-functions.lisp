;; missing-functions.lisp
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

(defmacro multf (place &optional (delta 1))
  (let ((del (gensym)))
    `(let ((,del ,delta))
       (setf ,place (.* ,place ,del)))))

(declaim (inline 1/))
(defun 1/ (number)
  (/ 1 number))

(declaim (inline eps=))
(defun eps= (a b)
  "Determines if a is within double-float precision of b"
  (and (> a (- b double-float-epsilon))
       (< a (+ b double-float-epsilon))))
