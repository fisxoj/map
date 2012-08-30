(in-package #:map)

(defparameter *gnuplot-path* #p"/usr/bin/gnuplot")
(defparameter *gnuplot-args* '("-persist"))
(defparameter *gnuplot-process* nil)

(defmacro with-plot (stream &body body)
  `(let ((gnuplot (ensure-gnuplot-connection)))
     (unwind-protect
	  (with-open-stream (,stream (external-program:process-input-stream gnuplot))
	    (format ,stream "set term wx~%")
	    ,@body)
;       (external-program:process-close gnuplot)
)))

(defun close-gnuplot-connection ()
  (when *gnuplot-process*
    (external-program:signal-process *gnuplot-process* :quit)))

(defun ensure-gnuplot-connection ()
  (if (or (null *gnuplot-process*) 
	  (not (eq (external-program:process-status *gnuplot-process*) :running)))
      (setf *gnuplot-process* (external-program:start *gnuplot-path* *gnuplot-args*
				    :input :stream))
      *gnuplot-process*))

(defun parse-style (style-alist)
  (with-output-to-string (s)
    ;; Do we have a title?  Print it first
    (when (assoc :title style-alist)
      (format s " title ~S" (cadr (assoc :title style-alist)))
      (setf style-alist (remove-if (lambda (k) (eq (car k) :title)) style-alist)))

    ;; More style directives?  They live together
    (when style-alist
      (format s " with ")
      (loop for (key value) in style-alist
	 do (cond
	      ((eq key :line-style)
	       (format s "~a" (cond
			      ((eq value :line) "line")
			      ((eq value :line-points) "linespoints"))))
	      ((eq key :yerr)
	       (format s " yerr ")))))))

(defun print-vectors-to-stream (stream x y)
	  (loop for xi across x
	       for yi across y
	     do (format stream "~1,8f ~1,8f~%" xi yi))
	  (format stream "e~%"))

(defun plot (&rest args)
  ;; Let's say things are (x y title) for now
  (with-plot stream
    (if (consp (car args))

	;; List of things to plot
	(progn
	  (loop for plot in (car args)
	     do (format stream "plot '-' using 1:2 ~a" (parse-style (third plot)))
	     unless (eql plot (car (last (car args))))
	     do (format stream ","))
	       
	  (loop for plot in (car args)
	     do (destructuring-bind (x y nil) plot
		  (print-vectors-to-stream stream x y)))) ; hah! second-class functions to the rescue!
	

	;; One thing to plot, yay!
	(let ((x (first args))
	      (y (second args))
	      (style (parse-style (third args))))
	  (format stream "plot '-' using 1:2 ~a~%" style)
	  (print-vectors-to-stream stream x y)))))

(defun image (data)
  (with-plot s
    (format s "set pm3d map~%")
    (format s "splot '-' matrix~%")
    (loop for i from (1- (array-dimension data 0)) downto 0
       do (loop for j from (1- (array-dimension data 1)) downto 0
	     do (format s "~g " (aref data i j)))
       do (format s "~%"))
    (format s "e~%")))

(defun plot3d (data)
;  (declare (type real-matrix data))
  (with-plot s
;    (format s "set pm3d~%")
    (format s "set isosample 4~%")
    (format s "splot '-'~%")
    (loop for i from 0 upto (1- (array-dimension data 0))
	 do (loop for j from 0 upto (1- (array-dimension data 1))
		 do (format s "~g " (aref data i j)))
	 do (format s "~%"))
    (format s "e~%")))
