(in-package #:map)
#|
(defmacro loop-over-matrix (M indices &body body)
  (let ((dim (array-dimensions M)) (index (car indices)))
    (loop for index in indices
	 for i from 0 by 1)
    (if (not (cdr indices))
	`(progn ,@body)
	(loop for ,index from 0 upto (array-dimension))))
  (loop
     for var in indices
     for i from 0 by 1
       
  ))
|#

(defmacro compose-matrix-loop (M indices &body body)
  (inner-c-m-l M indices 0 body))

(defmacro inner-c-m-l (M indices index &body body)
  (if (not (= index (length indices)))
      `(loop for ,(nth index indices) from 0 upto ,(array-dimension M index)
	  do ,(inner-c-m-l M indices (1+ index) body))
      `(progn ,@body)))
#|
(defmacro loop-over-vector (V index &body body)
  `(loop for ,index from 0 upto ,(length V)
      do (progn ,@body)))
|#
