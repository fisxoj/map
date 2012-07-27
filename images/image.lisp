(in-package #:map)

(defun imread (pathname)
  (multiple-value-bind  (image width height color-channels) (jpeg:decode-image pathname)
    (make-array (list width height color-channels)
		:displaced-to image :displaced-index-offset 0)))
