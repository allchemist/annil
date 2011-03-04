(in-package :annil)

(defun quickprop-1 (w slopes-fn patterns params)
  (let ((N (num-patterns patterns))
	(mu (param params :mu)))
    (let ((prev-slopes (make-matrix-like w))
	  (params (copy-tree params))
	  (err most-positive-fixnum)
	  (prev-err nil)
	  (verbosity (param params :verbosity))
	  (eps (/ (param params :eps) N))
	  (shrink-factor (/ mu (1+ mu)))
	  (delta-weights (make-matrix-like w)))
      ;; main loop
      (dotimes (i (param params :epochs))
	(let ((slopes (make-matrix-like w)))
	  
	  (setf prev-err err
		err (nth-value 1 (funcall slopes-fn weights slopes)))
	  (map-three-matrices delta-weights slopes prev-slopes
			      #'(lambda (d s ps)
				  (quickprop-update d s ps eps mu shrink-factor)))
	  (m+ w delta-weights)
	  (setf prev-slopes slopes)
	  (setf err (/ err N))
	  (when (>= verbosity 3)
	    (info "Current result: ~A~%" err))))
      (values w err))))
