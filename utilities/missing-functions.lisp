(in-package :map)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; missing-functions.lisp
;;
;; (c) 2012 Matt Novenstern <fisxoj@gmail.com>
;;
;; This file is for things I come across that feel like they should be in the standard
;; lisp library, but aren't.  They should be very much in the style of standard lisp
;; functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro multf (place &optional (delta 1))
  (let ((del (gensym)))
    `(let ((,del ,delta))
       (setf ,place (* ,place ,del)))))

(defun 1/ (number)
  (/ 1 number))
