(in-package #:map)

(export '(same-size-p
	  squarep
	  hermitianp))

(defun same-size-p (A B)
  (loop for a in (mapcar #'= (array-dimensions A) (array-dimensions B))
       when (not a) do (return nil)
       finally (return t)))

(defun inner-dimensions-match-p (A B)
  (let ((dimA (array-dimensions A))
	(dimB (array-dimensions B)))
    (and (= (second dimA) (first dimB)))))

(defun squarep (M)
  (apply #'= (array-dimensions M)))

(defun hermitianp (M)
  (and (loop for i from 0 to (1- (array-dimension M 0))
	  collect (loop for j from 0 to (- (array-dimension M 1) i 1)
		     collect (= (conjugate (aref M i j)) (aref M j i))))))
