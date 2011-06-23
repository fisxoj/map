(defun vectorize-macro (stream subchar arg)
  (declare (ignore subchar arg))
  (let* ((sexp (read stream t))
	 (xi (car sexp))
	 (xf (or (caddr sexp) (cadr sexp)))
	 (dx (cadr sexp)))
    (if (caddr sexp)
	(vectorize xi dx xf)
	(vectorize xi (signum (- xf xi)) xf))))

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
	   with vector = (make-array (1+ size) :element-type 'single-float)
	   for step from xi to xf by dx
	   for i from 0 by 1
	   do (setf (aref vector i) step)
	   finally (return vector))
	(loop with size = (1+ (ceiling (- xf xi))) ;; [xi:xf] equivalent
	   with vector = (make-array size :element-type 'single-float)
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

(defmethod vectorize ((xi single-float) (dx single-float) (xf single-float))
  (loop with size = (1+ (ceiling (/ (- xf xi) dx))) ;; [xi:xf] equivalent
     with vector = (make-array size :element-type 'single-float)
     for step from xi to xf by dx
     for i from 0 by 1
     do (setf (aref vector i) step)
     finally (return vector)))

(defmethod vectorize (xi dx xf)
  (let ((xii (coerce xi 'single-float))
	(dxi (coerce dx 'single-float))
	(xfi (coerce xf 'single-float)))
    (loop with size = (1+ (ceiling (/ (- xfi xii) dxi))) ;; [xi:xf] equivalent
       with vector = (make-array size :element-type 'single-float)
       for step from xii to xfi by dxi
       for i from 0 by 1
       do (setf (aref vector i) step)
       finally (return vector))))

(set-dispatch-macro-character #\# #\: #'vectorize-macro)