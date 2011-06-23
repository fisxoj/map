(defun same-size-p (A B)
  (loop for a in (mapcar #'= (array-dimensions A) (array-dimensions B))
       when (not a) do (return nil)
       finally (return t)))
