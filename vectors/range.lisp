;; range.lisp
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

(defclass range ()
  ((start :accessor range-start :initarg :start)
   (delta :accessor range-delta :initarg :delta)
   (stop  :accessor range-stop  :initarg :stop)))

(defun rangep (object)
  (typep object 'range))

(defun range-length (range)
  (the integer (1+ (round (/ (- (range-stop range) (range-start range)) (range-delta range))))))

(defun range-macro (stream subchar arg)
  (declare (ignore subchar arg))
  (let* ((sexp (read stream t)))
    (ecase (length sexp)
      (3 `(make-instance 'range :start ,(first sexp) :delta ,(second sexp) :stop ,(third sexp)))
      (2 `(make-instance 'range :start ,(first sexp) :delta ,(max (signum (- (second sexp) (first sexp))) 1) :stop ,(second sexp))))))

(set-dispatch-macro-character #\# #\r #'range-macro)

