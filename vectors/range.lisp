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
  (let* ((sexp (read stream t))
	 (xi (first sexp))
	 (xf (or (third sexp) (second sexp)))
	 (dx (second sexp)))
    (if (caddr sexp)
	`(make-instance 'range :start ,xi :delta ,dx :stop ,xf)
	`(make-instance 'range :start ,xi :delta ,(signum (- xf xi)) :stop ,xf))))

(set-dispatch-macro-character #\# #\r #'range-macro)

