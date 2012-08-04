(in-package #:map)

(defun (setf mref) (new-array array &rest subscripts)
  (mref-iter new-array array subscripts (copy-list subscripts) 0))

(defun mref-iter (new-array array subscripts start-subscripts index)
  (if (not (= (length subscripts) index))
      (let ((sub (nth index subscripts))
	    (new-subscripts (copy-list subscripts)))
	(if (typep sub 'range)
	    ;; If subscript is a range, iterate over it
	    (loop for i from (range-start sub) to (range-stop sub) by 1
	       initially (setf (nth index start-subscripts) (range-start sub))
	       do (setf (nth index new-subscripts) i)
	       do (mref-iter new-array array new-subscripts start-subscripts (1+ index)))
	    ;; If it's just an integer, pass it along, no need to loop
	    (mref-iter new-array array new-subscripts start-subscripts (1+ index))))
      ;; All subscripts are specified, set the value!
      (progn
	(print (mapcar #'- subscripts start-subscripts))
	  (setf (apply #'aref array subscripts)
		(apply #'aref new-array (subseq (mapcar #'- subscripts start-subscripts)
						   0 (array-rank new-array)))))))
