(in-package :annil)

(defun sse-compute-slopes (weights slopes patterns act-fn deriv-fn params)
  (let ((lgrad (make-matrix (patterns-output-dim patterns)))
	(out (make-matrix (patterns-output-dim patterns)))
	(err 0.0)
	(deriv-offset (param params :deriv-offset)))
    (do-patterns-safe (patterns p)
      ;; calculate local grad and error
      (map-matrix (gemv weights (first p) :dest out) act-fn)
      (m- (copy out lgrad) (second p))
      (%incf err (%square (e-norm lgrad)))
      ;;;; count error bits
      ;;(when classify-range
      ;;  (%dotimes (i (dim0 lgrad))
      ;;    (when (> (abs (%fvref lgrad i)) classify-range)
      ;;      (incf err-bits))))
      ;; calculate slopes
      (map-two-matrices lgrad out
			#'(lambda (x y)
			    (declare (type single-float x y))
			    (the single-float
			      (%* x (if deriv-offset
					(%+ (funcall deriv-fn y) deriv-offset)
					(funcall deriv-fn y))))))
      ;; modify slopes
      (m+ slopes (ger lgrad (first p))))
    (values slopes err)))

(defun optimize-sse (weights patterns test-part act-fn params)
  (let ((deriv-fn (deriv-fn-name act-fn))
	(method (or (param params :method) :quickprop))
	train test)
    (if (or (null test-part) (zerop test-part))
	(setf train patterns)
	(multiple-value-setq (train test)
	  (cv-split-patterns patterns test-part)))
    (flet ((slopes-fn (w s)
	     (declare (type (simple-array single-float) s w))
	     (sse-compute-slopes w s train act-fn deriv-fn params))
	   (err-fn (w)
	     (declare (type (simple-array single-float) w))
	     (sse-compute-errors w (or test train) act-fn params)))
      (optimize-with-restarts weights
        method #'slopes-fn #'err-fn train test params))))

(defun sse-compute-errors (weights patterns act-fn params)
  (declare (ignore params))
  (let ((out (make-matrix (patterns-output-dim patterns)))
	(err 0.0))
    (do-patterns-safe (patterns p)
      ;; calculate error
      (map-matrix (gemv weights (first p) :dest out) act-fn)
      (m- out (second p))
      (%incf err (%square (e-norm out)))
      ;;;; count error bits
      ;;(when classify-range
      ;;  (dotimes (i (dim0 out))
      ;;    (when (< (aref out i) classify-range)
      ;;      (incf err-bits)))))
      err)))
