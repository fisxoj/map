;; plot.lisp
;;
;; Copyright (c) 2012 Matt Novenstern <fisxoj@gmail.com>
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 3 of
;; the GNU Affero General Public License as published by
;; the Free Software Foundation.
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose. See the GNU
;; Affero General Public License for more details.
;;
;; Version 3 of the GNU Affero General Public License is in the file
;; LICENSE that was distributed with this file.
;; If it is not present, you can access it from
;; https://www.gnu.org/licenses/agpl.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA
;;

(in-package #:map)

(defparameter *gnuplot-path* #p"/usr/bin/gnuplot")
(defparameter *gnuplot-args* '("-persist"))
(defparameter *gnuplot-process* nil)
;; Thanks http://yujianzhang.blogspot.com/2007/12/nicer-fonts-for-gnuplot.html
(defparameter *gnuplot-font* "/usr/share/fonts/bitstream-vera/Vera.ttf,12")

(defmacro with-plot ((stream &optional terminal pathspec) &body body)
  (when (and (member terminal '(:eps :ps :png :jpg))
	     (not pathspec))
    (error "Attempting to plot to a file with no pathspec given"))
  `(let ((gnuplot (ensure-gnuplot-connection)))
     (with-open-stream (,stream (external-program:process-input-stream gnuplot))
					;with-open-file (,stream #p"./test" :direction :output :if-exists :supersede)
       (format ,stream "set term ~a enhanced font ~S~%" (string-downcase ,terminal) *gnuplot-font*)
       (when ,pathspec (format ,stream "set output ~S~%" (namestring ,pathspec)))
       ,@body)))

(defun close-gnuplot-connection ()
  (when *gnuplot-process*
    (external-program:signal-process *gnuplot-process* :quit)))

(defun ensure-gnuplot-connection ()
  (if (or (null *gnuplot-process*) 
	  (not (eq (external-program:process-status *gnuplot-process*) :running)))
      (setf *gnuplot-process* (external-program:start *gnuplot-path* *gnuplot-args*
				    :input :stream))
      *gnuplot-process*))

(defun parse-style (stream style-number args)
  ;; Do we have a title?  Print it first

  (format stream "set style line ~d " style-number)
  ;; More style directives?  They live together

  (if args
      (destructuring-bind (&key line-type line-color line-width point-type point-size &allow-other-keys) args
	(when line-type (format stream "lt ~a " line-type))
	(when line-color (format stream "lc ~a " line-color))
	(when line-width (format stream "lw ~a " line-width))
	(when point-type (format stream "pt ~a " point-type))
	(when point-size (format stream "ps ~a " point-size))
	(format stream "~%"))
      (format stream "default~%")))

(defun print-vectors-to-stream (stream x y)
	  (loop for xi across x
	       for yi across y
	     do (format stream "~1,8f ~1,8f~%" xi yi))
	  (format stream "e~%"))

(defun plot (plot-sexps &key title (terminal :wxt) (pathspec "") x-label y-label)
  ;; Make sure args is always a list of plot specs
  (let ((plot-sexps (if (consp (car plot-sexps))
			plot-sexps
			(list plot-sexps))))

    (with-plot (stream terminal pathspec)
      (when title (format stream "set title ~S~%" title))
      (when x-label (format stream "set xlabel ~S~%" x-label))
      (when y-label (format stream "set ylabel ~S~%" y-label))

      ;; Write line styles to stream
      (loop
	 for plot in plot-sexps
	 for number from 1 by 1
	 do (parse-style stream number (subseq plot 2)))

      ;; Write plot commands to stream
      (loop
	 for plot in plot-sexps
	 for number from 1 by 1
	 initially (princ "plot " stream)
	 do (format stream
		    "'-' using 1:2 ls ~d title ~S with ~a"
		    number
		    (or (getf plot :title) "")
		    (case (getf plot :line-style)
		      (:lines "lines")
		      (:points "points")
		      (:lines-points "linespoints")
		      (:impulses "impulses")
		      (:dots "dots")
		      (:steps "steps")
		      ;; FIXME: Add others once we can parse more vectors (errorbars)
		      (t "lines")))
	 unless (= (length plot-sexps) number)
	 do (princ ", " stream)
	 finally (princ #\Newline stream))

      
      (loop
	 for plot in plot-sexps
	 do (destructuring-bind (x y) (subseq plot 0 2)
	      ;; hah! second-class functions to the rescue!
	      (print-vectors-to-stream stream x y))))))

(defun image (data &key title (terminal :wxt) (pathspec "") x-label y-label)
  (with-plot (s terminal pathspec)
    (when title (format s "set title ~S~%" title))
    (when x-label (format s "set xlabel ~S~%" x-label))
    (when y-label (format s "set ylabel ~S~%" y-label))

    (format s "set pm3d map~%")
    (format s "set size ratio 1~%")
    (format s "splot '-' matrix~%")
    (loop for i from (1- (array-dimension data 1)) downto 0
       do (loop for j from 0 upto (1- (array-dimension data 0))
	     do (format s "~g " (aref data j i)))
       do (format s "~%"))
    (format s "e~%")))

(defun plot3d (data &key title (terminal :wxt) (pathspec "") x-label y-label z-label)
;  (declare (type real-matrix data))
  (with-plot (s terminal pathspec)
    (when title (format s "set title ~S~%" title))
    (when x-label (format s "set xlabel ~S~%" x-label))
    (when y-label (format s "set ylabel ~S~%" y-label))
    (when z-label (format s "set zlabel ~S~%" z-label))

;    (format s "set pm3d~%")
    (format s "set isosample 4~%")
    (format s "splot '-' matrix~%")
    (loop for i from 0 below (array-dimension data 0)
       do (loop for j from 0 below (array-dimension data 1)
	     do (format s "~g " (aref data i j)))
       do (format s "~%"))
    (format s "e~%")))
