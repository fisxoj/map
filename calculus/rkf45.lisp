;; rkf45.lisp
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

(defun rkf45 (func range y0 &optional (tolerance 1d-7))
  "rkf45
Implementation of the Runge-Kutta-Feldberg differential equation solver.

func: function handle that should accept a time, ti, and a Nx2
t-range: a map:range object that contains the start and stop values and a suggested step as the interval.  Since the algorithm picks its own optimal step, it will use that value as guidance only.

tolerance: tolerance"
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let* ((array-width (length (the vector y0)))
	 (array-length 100)
	 (ys (make-array (list array-length array-width) :element-type 'double-float :adjustable t))
	 (ts (make-array array-length :element-type 'double-float :adjustable t))
	 (step (/ (- (range-stop range) (range-start range)) 1d2)))
    (declare (type function func)
	     (type double-float tolerance step)
	     (type (simple-vector *) y0))
    ;; Set initial conditions from y0
    (setf (aref ts 0) (range-start range))
    (loop for j from 0 upto (1- array-width)
	 do (setf (aref ys 0 j) (aref y0 j)))
    (loop
       ;; Just keep track of which element we're on...
       for i from 1 by 1;; upto (1- array-length)
       for ti = (the double-float (elt ts (1- i)))
       ;; Collect last step's y-values in a vector
       for yi = (make-array array-width
			    :element-type 'double-float
			    :displaced-to ys
			    :displaced-index-offset (* array-width (1- i)))

       ;; Stop at the end of the given interval
       until (>= ti (the double-float (range-stop range)))
;       when (zerop (mod i 1000)) do (format t "Step size: ~g (time: ~d)~%" step ti)
       do (setf array-length i)
       do (loop
	     with looped = nil
;;	     do (format t "Step size: ~g~%" step)
	     do (let* ((k1 (.* step (funcall func ti yi)))
		       (k2 (.* step (funcall func (+ ti (* .25d0 step)) (.+ yi (.* .25d0 k1)))))
		       (k3 (.* step (funcall func (+ ti (* .375d0 step))
					     (reduce #'.+ (list yi (.* .09375d0 k1) (.* 0.28125d0 k2))))))
		       (k4 (.* step (funcall func (+ ti (* 0.9230769230769231d0 step))
					     (reduce #'.+ (list yi
								(.* 0.8793809740555303d0 k1) 
								(.* -3.277196176604461d0 k2)
								(.* 3.3208921256258535d0 k3))))))
		       (k5 (.* step (funcall func (+ ti step)
					     (reduce #'.+ (list yi
								(.* 2.0324074074074074d0 k1)
								(.* -8d0 k2) (.* 7.173489278752436d0 k3)
								(.* -0.20589668615984405d0 k4))))))
		       (k6 (.* step (funcall func (+ ti (* .5d0 step))
					     (reduce #'.+ (list yi
								(.* -0.2962962962962963d0 k1)
								(.* 2d0 k2)
								(.* -1.3816764132553607d0 k3)
								(.* 0.4529727095516569d0 k4)
								(.* -0.275d0 k5))))))
		       (rk4 (the (simple-array double-float *)
			      (reduce #'.+ (list yi
						 (.* 0.11574074074074074d0 k1)
						 (.* 0.5489278752436647d0 k3)
						 (.* 0.5353313840155945d0 k4)
						 (.* -.2d0 k5)))))
		       (rk5 (the (simple-array double-float *)
			      (reduce #'.+ (list yi
						 (.* 0.11851851851851852d0 k1)
						 (.* 0.5189863547758284d0 k3)
						 (.* 0.5061314903420167d0 k4)
						 (.* -0.18d0 k5)
						 (.* 0.03636363636363636d0 k6)))))
		       (err (/ (max (the double-float (norm (.- rk5 rk4))) double-float-epsilon)
			       tolerance)))
		  (declare (type double-float err))
		  ;; If err is less > one, the step is not precise enough
		  ;; if it's smaller than .1, it's too precise!
		  ;; also, don't adjust the size more than once...
	(if (and (not looped)
		 (or (> (abs err) 1d0)
		     ;; I'm pretty ok with things being a bit too precise for now
		     (< (abs err) 1d-3)))
	    (setf step (* step 8d-1 (the double-float (expt (* 2d0 err) -1/6)))
		  looped t)
	    
	    ;; Error is low enough, keep the value
	    (progn
	      ;; If there's not enough space, make the arrays longer

	      (when (= (1- (array-dimension ys 0)) i)
		(adjust-array ys (list (* 2 (array-dimension ys 0)) array-width))
		(adjust-array ts (* 2 (length ts))))

	      (setf (aref ts i) (+ (the double-float ti) step))
	      (loop for j from 0 upto (1- array-width)
		 do (setf (aref ys i j) (elt rk5 j)))
	      (return)))
	)))
    (values (adjust-array ts array-length) 
	    (adjust-array ys (list array-length array-width)))
))
