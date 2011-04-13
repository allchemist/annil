(in-package :annil)

(export '(quickprop quickprop-single))

(defun quickprop-update (d s ps eps mu shrink-factor)
  "Quickprop update rule, choose linear or quadratic estimate out of slope size"
  (let ((step 0.0))
    (cond ((minusp d)
	   (when (plusp s)
	     (decf step (* eps s))) 
	   (if (>= s (* shrink-factor ps))
	       (incf step (* mu d))
	       (incf step (* d (/ s (- ps s))))))
	  ((plusp d)
	   (when (minusp s)
	     (decf step (* eps s)))
	   (if (<= s (* shrink-factor ps))
	       (incf step (* mu d))
	       (incf step (* d (/ s (- ps s))))))
	  (t (decf step (* eps s))))
    step))

(defun quickprop (w slope-fn err-fn train-patterns test-patterns params)
  (let ((restarts (or (param params :restarts) 0))
	(err most-positive-fixnum)
	(epochs 0)
	(copy-params (copy-tree params)))
    (decf (param copy-params :verbosity))
    (dotimes (i (1+ restarts))
      (multiple-value-bind (cur-w cur-err cur-epochs)
	  (quickprop-single w slope-fn err-fn train-patterns test-patterns copy-params)
	(setf (param copy-params :eps) (/ (param copy-params :eps) 2))
	(setf (param copy-params :mu) (/ (param copy-params :mu) 2))
	(setf (param copy-params :thr) (/ (param copy-params :thr) 5))
	(setf err cur-err)
	(incf epochs cur-epochs)))
    (incf (param copy-params :verbosity))
    (values w err epochs)))

(defun quickprop-single (weights slopes-fn err-fn train-patterns test-patterns init-params)
  (flet ((w-like () (make-matrix-like weights)))
    (let ((prev-slopes (w-like))
	  (delta-weights (w-like))
	  (params (copy-tree init-params))
	  (num-patterns (num-patterns train-patterns))
	  (test-num-patterns (when test-patterns (num-patterns test-patterns)))
	  (epoch-err most-positive-fixnum)
	  (prev-epoch-err nil)
	  (test-err most-positive-fixnum)
	  (prev-test-err nil)
	  (epochs-passed 0)
	  (verbosity (param init-params :verbosity))
	  (recompute-limit (param init-params :recompute))
	  (thr (or (param init-params :thr) 0.0))
	  (recompute-flag nil)
	  (best-weights (w-like))
	  (best-err (float most-positive-fixnum)))
      ;; parameters checking
      (assert (typep (param params :epochs) 'fixnum) nil "Number of iterations not integer")
      (when (or (not (typep verbosity 'fixnum))
		(minusp verbosity))
	(setf verbosity 0))
      (when (> verbosity 3) (setf verbosity 3))
      (unless (param params :eps)
	(when (>= verbosity 3) (info "Using default parameter: eps = 0.1~%"))
	(setf (param params :eps) 0.1))
      (unless (param params :mu)
	(when (>= verbosity 3) (info "Using default parameter: mu = 2.0~%"))
	(setf (param params :mu) 2.0))
      (unless recompute-limit
	(when (>= verbosity 3) (info "Using unlimited recomputation limit~%")))
      ;; main loop
      (dotimes (i (param params :epochs))
	(incf epochs-passed)
	(let ((slopes (w-like))
	      (eps (/ (param params :eps) num-patterns))
	      (mu (param params :mu))
	      (shrink-factor (/ (param params :mu) (1+ (param params :mu)))))
;;; ========================================================
	  (setf prev-epoch-err epoch-err
		epoch-err (/ (nth-value 1 (funcall slopes-fn weights slopes)) num-patterns))
	  (when (< epoch-err best-err)
	    (setf best-err (if test-patterns test-err epoch-err))
	    (copy weights best-weights))
	  (map-three-matrices delta-weights slopes prev-slopes
			      #'(lambda (d s ps)
				  (quickprop-update d s ps eps mu shrink-factor)))
	  (m+ weights delta-weights)
	  (setf prev-slopes slopes)
;;; ========================================================
	  (when test-patterns
	    (setf prev-test-err test-err
		  test-err (/ (funcall err-fn weights) test-num-patterns)))
	  (when (>= verbosity 3)
	    (if test-patterns
		(info "Current result: ~A, over test patterns: ~A~%" epoch-err test-err)
		(info "Current result: ~A~%" epoch-err)))
	   (when recompute-limit
	     (when (if test-patterns
		       (<= prev-test-err test-err)
		       (<= prev-epoch-err epoch-err))
	       (setf recompute-flag t))
	     (if (if test-patterns
		     (<= prev-test-err (* (1+ thr) test-err))
		     (<= prev-epoch-err (* (1+ thr) epoch-err)))
		 (when recompute-flag (decf (param params :recompute)))
		 (when recompute-flag
		   (when (< (param params :recompute) recompute-limit)
		     (incf (param params :recompute) 1/2))))
	     (when (minusp (param params :recompute))
	       (when (>= verbosity 1) (info "Stagnant at ~A epoch~%" i))
	       (return)))))
      (when (>= verbosity 1)
	(if test-patterns
	    (info "Final result: ~A, over test patterns: ~A~%" best-err test-err)
	    (info "Final result: ~A~%" best-err)))
      (values best-weights best-err epochs-passed))))
