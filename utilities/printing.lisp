(in-package #:map)

(defmacro mute (&body body)
  `(progn ,@body
	  nil))
