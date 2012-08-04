(in-package #:map)

(defun rkf45 (func range y0 &optional (tolerance 1d-7))
  "rkf56
Implementation of the Runge-Kutta-Feldberg differential equation solver.

func: function handle that should accept a time, ti, and a Nx2
t-range: a map:range object that contains the start and stop values and a suggested step as the interval.  Since the algorithm picks its own optimal step, it will use that value as guidance only.

tolerance: tolerance"
  (let* ((array-width (length (the vector y0)))
	 (array-length (the integer (range-length range)))
	 (ys (make-array (list array-length array-width) :element-type 'double-float))
	 (ts (make-array array-length :element-type 'double-float))
	 (errs (make-array array-length :element-type 'double-float))
	 (step (the double-float (range-delta range))))
    ;; Set initial conditions from y0
    (setf (aref ts 0) (range-start range))
    (loop for j from 0 upto (1- array-width)
	 do (setf (aref ys 0 j) (aref y0 j)))
    (loop
       ;; Just keep track of which element we're on...
       for i from 1 upto (1- array-length)
       for ti = (the double-float (elt ts (1- i)))
       ;; Collect last step's y-values in a vector
       for yi = (make-array array-width
			    :element-type 'double-float
			    :displaced-to ys
			    :displaced-index-offset (* array-width (1- i)))

       ;; Stop at the end of the given interval
;       until (>= ti (the double-float (range-stop range)))

       do (loop
	     do (let* ((k1 (.* step (funcall (the function func) ti yi)))
		       (k2 (.* step (funcall (the function func) (+ ti (* 1/4 step)) (.+ yi (.* 1/4 k1)))))
		       (k3 (.* step (funcall (the function func) (+ ti (* 3/8 step)) (reduce #'.+ (list yi (.* 3/32 k1) (.* 9/32 k2))))))
		       (k4 (.* step (funcall (the function func) (+ ti (* 12/13 step)) (reduce #'.+ (list yi (.* 1932/2197 k1) (.* -7200/2197 k2) (.* 7296/2197 k3))))))
		       (k5 (.* step (funcall (the function func) (+ ti step) (reduce #'.+ (list yi (.* 439/216 k1) (.* -8 k2) (.* 3680/513 k3) (.* -845/4104 k4))))))
		       (k6 (.* step (funcall (the function func) (+ ti (* 1/2 step)) (reduce #'.+ (list yi (.* -8/27 k1) (.* 2 k2) (.* -3544/2565 k3) (.* 1859/4104 k4) (.* -11/40 k5))))))
		       (rk4 (the (simple-array double-float *) (reduce #'.+ (list yi (.* 25/216 k1) (.* 1408/2565 k3) (.* 2197/4104 k4) (.* -1/5 k5)))))
		       (rk5 (the (simple-array double-float *) (reduce #'.+ (list yi (.* 16/135 k1) (.* 6656/12825 k3) (.* 28561/56430 k4) (.* -9/50 k5) (.* 2/55 k6)))))
		       (err (norm (.- rk5 rk4))))
		  (when (< (the double-float err) (the double-float tolerance))
		    (setf (aref ts i) (+ (the double-float ti) (the double-float step)))
		    (loop for j from 0 upto (1- array-width)
		       do (setf (aref ys i j) (elt rk5 j)))
		    (setf (aref errs i) err)
		    (return))
		  (setf step (* step (expt (/ tolerance err 2d0) 1/4)))
		  )))
    (values ts ys)))
