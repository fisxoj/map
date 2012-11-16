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

