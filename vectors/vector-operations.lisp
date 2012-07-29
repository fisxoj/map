(in-package #:map)

(defmethod norm ((vec vector))
  (sqrt (loop for i across vec sum (* i i))))
