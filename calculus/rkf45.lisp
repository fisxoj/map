(in-package #:map)

(defun rkf45 (func range y0 &optional (tolerance 1d-7))
  "rkf56
Implementation of the Runge-Kutta-Feldberg differential equation solver.

func: function handle that should accept a time, ti, and a Nx2
t-range: a map:range object that contains the start and stop values and a suggested step as the interval.  Since the algorithm picks its own optimal step, it will use that value as guidance only.

tolerance: tolerance"
  (let ((ys (loop for i from 0 to (1- (length y0)) collect (make-array 10 :element-type 'double-float :adjustable t :fill-pointer 0)))
	(ts (make-array 10 :element-type 'double-float :adjustable t :fill-pointer 0))
	(errs (make-array 10 :element-type 'double-float :adjustable t :fill-pointer 0))
	(step (range-delta range)))
    ;; Set initial conditions from y0
    (loop for i from 0 to (1- (length y0)) do (vector-push-extend (elt y0 i) (nth i ys)))
    (vector-push-extend (range-start range) ts)

    (loop
       ;; Just keep track of which element we're on...
       for i from 0 by 1
       for ti = (elt ts i)
       ;; Collect last step's y-values in a vector
       for yi = (coerce (loop for j from 0 to (1- (length ys))
			   collect (aref (nth j ys) i))
			'vector)

       ;; Stop at the end of the given interval
       until (>= ti (range-stop range))

       do (loop
	     do (let* ((k1 (.* step (funcall func ti yi)))
		       (k2 (.* step (funcall func (+ ti (* 1/4 step)) (.+ yi (.* 1/4 k1)))))
		       (k3 (.* step (funcall func (+ ti (* 3/8 step)) (reduce #'.+ (list yi (.* 3/32 k1) (.* 9/32 k2))))))
		       (k4 (.* step (funcall func (+ ti (* 12/13 step)) (reduce #'.+ (list yi (.* 1932/2197 k1) (.* -7200/2197 k2) (.* 7296/2197 k3))))))
		       (k5 (.* step (funcall func (+ ti step) (reduce #'.+ (list yi (.* 439/216 k1) (.* -8 k2) (.* 3680/513 k3) (.* -845/4104 k4))))))
		       (k6 (.* step (funcall func (+ ti (* 1/2 step)) (reduce #'.+ (list yi (.* -8/27 k1) (.* 2 k2) (.* -3544/2565 k3) (.* 1859/4104 k4) (.* -11/40 k5))))))
		       (rk4 (reduce #'.+ (list yi (.* 25/216 k1) (.* 1408/2565 k3) (.* 2197/4104 k4) (.* -1/5 k5))))
		       (rk5 (reduce #'.+ (list yi (.* 16/135 k1) (.* 6656/12825 k3) (.* 28561/56430 k4) (.* -9/50 k5) (.* 2/55 k6))))
		       (err (norm (.- rk5 rk4))))
		  (when (< err tolerance)
		    (vector-push-extend (+ ti step) ts)
		    (loop for j from 0 upto (1- (length ys)) 
		       do (vector-push-extend (elt rk5 j) (nth j ys)))
		    (vector-push-extend err errs)
		    (return))
		  (setf step (* step (expt (/ tolerance err 2d0) 1/4)))
		  )))
    (values ts ys)))
