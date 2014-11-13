# Map

Map is a whole bunch of time-saving work for scientific computing.  It has Octave/Matlab style ranges, `#r(1 10)` and vectors `#v(1 10)` as well as matrix math functions, plotting, and a growing number of other things.

I will try to keep this updated with good examples, but I fully expect to be breaking everything over and over again for a while, so don't count on anything working any particular way for more than a few days.

Here's any example of a million things goin on at once.
```lisp
(map:plot #v(0 .1 (* 2 pi))
	  #v(0 .1 (* 2 pi) (lambda (n) (map:riemann #'sin #r(0 .01 n))))
	  '((:title "pizza")
	    (:line-style :line-points)))
```
The example does a 2d plot, which takes two vectors, the first being x values and the second being y-values.  Note that the second vector has a function passed to it, which is applied to the vector specified in.  Note that all elements specifying the vector are evaluated, so you can even write lambda functions to integrate! `plot` also accepts some style commands to render your plots the way you want!

## Syntactic sugar
### Vectors
```lisp
CL-USER> #v(1 10)
#(1 2 3 4 5 6 7 8 9 10)
```
Vectors will expand the numbers given in them into a set of evenly-spaced values, useful for passing to plotting functions or whatnot.  You can also optionally pass a delta value and a function to apply to the vector before returning it.
```lisp
CL-USER> #v(1 .5 10)
#(1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0 5.5 6.0 6.5 7.0 7.5 8.0 8.5 9.0 9.5 10.0)

CL-USER> #v(1 .5 10 #'sin)
#(0.8414709848078965 0.9974949866040544 0.9092974268256817 0.5984721441039565
  0.1411200080598672 -0.35078322768961984 -0.7568024953079282 -0.977530117665097
  -0.9589242746631385 -0.7055403255703919 -0.27941549819892586 0.21511998808781552
  0.6569865987187891 0.9379999767747389 0.9893582466233818 0.7984871126234903
  0.4121184852417566 -0.0751511204618093 -0.5440211108893698)
```

### Ranges
Some functions in map will accept a range object, which is simply a collection of start point, step value, and endpoint.  These are like vectors above, but don't expand and are useful for storing intervals, for things like specifying the range for an integration.

```lisp
CL-USER> (map:riemann #'sin #r(0d0 .1d0 pi))
1.999547959712597
```

### Integration
Map currently has two integration routines, a dumb, Riemann integration method, and the much smarter Runge-Kutta-Feldberg 45 method, which works for differential equations with order >= 1.

```lisp
;; Returns a vector of differential values to be multiplied by the step size
(defun damped-driven-pendulum (ti yi)
	    (declare (type double-float ti) (type (array double-float 2)))

	    (vector (+ (* 1.081d0 (cos (* 2 pi ti)))
	    	       (* (/ -3d0 4d0) pi (sin (elt yi 1)))
		       (* -1d0 3d0 pi (elt yi 0)))

		    (elt yi 0)))

;; Integrate and hide output so slime doesn't spend minutes just printing the result we
;; got in seconds

(map::mute (multiple-value-setq (ts ys)
	     (map:rkf45 #'damped-driven-pendulum
		        #r(0d0 1d-7 4d1)
			#(0d0 1d0) 1d-10)))

;; Plot (subject to heavy change)
;; the (mref) pulls out all of the values of the first axis (using t)
;; that are at 0 of the second index
(map:plot (list ts (map:mref ys t 0)))
```
