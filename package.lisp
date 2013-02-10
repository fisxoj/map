;; package.lisp
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

(defpackage #:map
  (:use #:cl)
  (:export
   ;; Missing functions
   #:multf
   #:1/

   ;; Matrix type definitions
   #:complex-matrix
   #:real-matrix
   #:matrix

   ;; Matrix math operation
   #:.+
   #:.-
   #:.*
   #:./
   #:.^

   ;; Matrix predicates
   #:same-size-p
   #:hermitianp
   #:squarep
   #:singularp

   ;; Matrix operations
   #:tensor-product
   #:norm
   #:eye
   #:zeros
   #:ones
   #:czeros
   #:random-matrix
   #:mtrace
   #:mapply
   #:mref
   #:determinant
   #:transpose
   #:inverse
   #:row-major-subscripts
   #:subscripts-row-major
   #:lu-decomposition

   ;; Matrix macros
   #:do-matrix
   #:as-vector

   ;; Vector operations
   #:norm
   #:dot
   #:mean
   #:standard-deviation
   #:sum

   ;; Calculus
   #:riemann
   #:rkf45

   ;; Images
   #:imread

   ;; Plotting
   #:plot
   #:plot3d
   #:image

   ;; Printing utility functions
   #:mute

   ;; Convenience functions/macros
   #:with-result
   ))

(in-package :map)
