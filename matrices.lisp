(defun zeros (&rest dimensions)
  (make-array dimensions :initial-element 0))

(defun eye (dimension)
  (loop with m = (zeros dimension dimension)
     for i from 0 to (1- dimension)
     do (setf (aref m i i) 1)
     finally (return m)))
