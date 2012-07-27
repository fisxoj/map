# Map

Map is a whole bunch of time-saving work for scientific computing.  It has Octave/Matlab style ranges, `#r(1 10)` and vectors `#v(1 10)` as well as matrix math functions, plotting, and a growing number of other things.

Here's any example of a million things goin on at once.
```lisp
(map:plot #v(0 .1 (* 2 pi))
	  #v(0 .1 (* 2 pi) (lambda (n) (map:riemann #'sin #r(0 .01 n))))
	  '((:title "pizza")
	    (:line-style :line-points)))
```
The example does a 2d plot, which takes two vectors, the first being x values and the second being y-values.  Note that the second vector has a function passed to it, which is applied to the vector specified in.  Note that all elements specifying the vector are evaluated, so you can even write lambda functions to integrate! `plot` also accepts some style commands to render your plots the way you want!