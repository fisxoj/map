(in-package #:map)

(defclass range ()
  ((start :accessor range-start :initarg :start)
   (delta :accessor range-delta :initarg :delta)
   (stop  :accessor range-stop  :initarg :stop)))

(defun vectorize-macro (stream subchar arg)
  (declare (ignore subchar arg))
  (let* ((sexp (read stream t))
	 (eval-last (eval (car (last sexp))))
	 (function (if (functionp eval-last) eval-last nil))
	 (xi (eval (first sexp)))
	 (xf (eval (cond 
		     ;; Function and 3 numbers
		     ((and (= (length sexp) 4) function)
		      (third sexp))
		     ;; No function and 3 numbers
		     ((and (= (length sexp) 3) (not function))
		      (third sexp))
		     ;; Function and 2 numbers
		     ((and (= (length sexp) 3) function)
		      (second sexp))
		     ;; No function, 2 numbers
		     ((= (length sexp) 2)
		      (second sexp)))))
	 (dx (eval (second sexp)))
	 (vector (if (or (and (= (length sexp) 4) function)
			 (and (= (length sexp) 3) (not function)))
		     (vectorize xi dx xf)
		     (vectorize xi (signum (- xf xi)) xf))))
    (if function
	(map 'vector function vector)
	vector)))

(defun range-macro (stream subchar arg)
  (declare (ignore subchar arg))
  (let* ((sexp (read stream t))
	 (xi (first sexp))
	 (xf (or (third sexp) (second sexp)))
	 (dx (second sexp)))
    (if (caddr sexp)
	`(make-instance 'range :start ,xi :delta ,dx :stop ,xf)
	`(make-instance 'range :start ,xi :delta ,(signum (- xf xi)) :stop ,xf))))


(defun vec-dbg (xi dx xf)
  (format t "xi: ~g  dx: ~g  xf: ~g~%" xi dx xf))

(defun vectorize2 (stream subchar arg)
  (declare (ignore subchar arg))
  (let* ((sexp (read stream t))
	 (xi (car sexp))
	 (xf (or (caddr sexp)
		 (cadr sexp)))
	 (dx (cadr sexp)))
    (if (caddr sexp)
	(loop with size = (1+ (ceiling (/ (- xf xi) dx))) ;; [xi:dx:xf] equivalent
	   with vector = (make-array (1+ size) :element-type 'double-float)
	   for step from xi to xf by dx
	   for i from 0 by 1
	   do (setf (aref vector i) step)
	   finally (return vector))
	(loop with size = (1+ (ceiling (- xf xi))) ;; [xi:xf] equivalent
	   with vector = (make-array size :element-type 'double-float)
	   for step from xi to xf by (signum (- xf xi))
	   for i from 0 by 1
	   do (setf (aref vector i) step)
	   finally (return vector)))))

(defgeneric vectorize (xi dx xf))

(defmethod vectorize ((xi integer) (dx integer) (xf integer))
  (loop with size = (1+ (ceiling (/ (- xf xi) dx))) ;; [xi:xf] equivalent
     with vector = (make-array size :element-type 'integer)
     for step from xi to xf by dx
     for i from 0 by 1
     do (setf (aref vector i) step)
     finally (return vector)))

(defmethod vectorize ((xi double-float) (dx double-float) (xf double-float))
  (loop with size = (1+ (ceiling (/ (- xf xi) dx))) ;; [xi:xf] equivalent
     with vector = (make-array size :element-type 'double-float)
     for step from xi to xf by dx
     for i from 0 by 1
     do (setf (aref vector i) step)
     finally (return vector)))

(defmethod vectorize (xi dx xf)
  (let ((xii (coerce xi 'double-float))
	(dxi (coerce dx 'double-float))
	(xfi (coerce xf 'double-float)))
    (loop with size = (1+ (ceiling (/ (- xfi xii) dxi))) ;; [xi:xf] equivalent
       with vector = (make-array size :element-type 'double-float)
       for step from xii to xfi by dxi
       for i from 0 by 1
       do (setf (aref vector i) step)
       finally (return vector))))

(set-dispatch-macro-character #\# #\v #'vectorize-macro)

(set-dispatch-macro-character #\# #\r #'range-macro)
