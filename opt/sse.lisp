(in-package :annil)

(export '(optimize-sse))

(declaim (ftype (function ((simple-array single-float) (simple-array single-float) t symbol)
			  (values (simple-array single-float) single-float))
		sse-compute-slopes))
(defun sse-compute-slopes (weights slopes patterns act-fn)
  (declare (optimize speed (safety 0))
	   (type (simple-array single-float) weights slopes))
  (let ((lgrad (the (simple-array single-float) (make-matrix (patterns-output-dim patterns))))
	(out (the (simple-array single-float) (make-matrix (patterns-output-dim patterns))))
	(err 0.0)
	(weights-p (patterns-with-weights-p patterns))
	(npats (float (num-patterns patterns))))
    (do-patterns-safe (patterns p i)
      ;; calculate local grad and error
      (activation (gemv weights (first p) :dest out) act-fn)
      (saxpy (second p) (copy out lgrad) -1.0) ;(m- (copy out lgrad) (second p))
      (when weights-p (sscal #|m-|# lgrad (%* (pattern-weight p) npats)))
      (%incf err (%square (e-norm lgrad)))
      ;;;; count error bits
      ;;(when classify-range
      ;;  (%dotimes (i (dim0 lgrad))
      ;;    (when (> (abs (%fvref lgrad i)) classify-range)
      ;;      (incf err-bits))))
      ;; calculate slopes
      (smap-two-matrices-* #|m*|# lgrad (activation-deriv out act-fn))
      ;; modify slopes
      (sger slopes lgrad (first p) 1.0 :noconj))
    (values slopes err)))

(defun sse-compute-errors (weights patterns act-fn)
  (declare (optimize speed (safety 0))
	   (type (simple-array single-float) weights))
  (let ((out (the (simple-array single-float) (make-matrix (patterns-output-dim patterns))))
	(err 0.0)
	(weights-p (patterns-with-weights-p patterns))
	(npats (float (num-patterns patterns))))
    (dotimes (i (num-patterns patterns))
      (let ((p (get-pattern-safe patterns i)))
	;; calculate error
	(activation (gemv weights (first p) :dest out) act-fn)
	(m- out (second p))
	(if weights-p
	    (%incf err (%* (%square (%* (pattern-weight p) npats))
			   (%square (e-norm out))))
	    (%incf err (%square (e-norm out))))
        ;;;; count error bits
	;;(when classify-range
	;;  (dotimes (i (dim0 out))
	;;    (when (< (aref out i) classify-range)
	;;      (incf err-bits)))))
	))
    err))

(defun optimize-sse (weights patterns test-part act-fn params)
  (let ((method (or (param params :method) :quickprop))
	train test)
    (if (or (null test-part) (zerop test-part))
	(setf train patterns)
	(multiple-value-setq (train test)
	  (cv-split-patterns patterns test-part (param params :test-sequential))))
    (flet ((slopes-fn (w s)
	     (declare (type (simple-array single-float) s w))
	     (sse-compute-slopes w s train act-fn))
	   (err-fn (w)
	     (declare (type (simple-array single-float) w))
	     (sse-compute-errors w (or test train) act-fn)))
      (optimize-with-restarts weights
        method #'slopes-fn #'err-fn train test params))))
