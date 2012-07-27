(in-package :map)

(defun riemann (func range)
  (loop for i from (range-start range)
     to (range-stop range)
     by (range-delta range)

     sum (* (range-delta range) (funcall func i))))
