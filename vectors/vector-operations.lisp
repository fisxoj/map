(in-package #:map)

(defmethod norm ((vec vector))
  (sqrt (loop for i across vec sum (* i i))))

(defun dot (a b)
  "Dot product of two vectors.  Returns a scalar."
  (assert (= (length a) (length b)))
  (reduce #'+ (map 'vector '* a b)))
