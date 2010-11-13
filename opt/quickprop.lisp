(in-package :annil)

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

(defun quickprop-sse (weights train-patterns test-patterns act-fn init-params)
  (flet ((w-like () (make-matrix-like weights)))
    (let (
	  ;; quickprop update space
	  (delta-weights (w-like))
	  (prev-delta-weights (w-like))
	  (slopes (w-like))
	  (prev-slopes (w-like))
	  ;; evaluate space
	  (out (make-matrix (patterns-output-dim train-patterns)))
	  (lgrad (make-matrix (patterns-output-dim train-patterns)))
	  ;; tmp bindings
	  (epoch-err most-positive-fixnum)
	  (prev-epoch-err 0)
	  (prev-test-err 0)
	  (test-err most-positive-fixnum)
	  (num-patterns (num-patterns train-patterns))
	  (act-fn-deriv (deriv-fn-name act-fn))
	  (last-epoch 0)
	  ;; params
	  (params (copy-tree init-params))
	  (deriv-offset (param init-params :deriv-offset))
	  (decay (param init-params :decay))
	  (recompute-limit (param init-params :recompute))
	  (adaptivity (param init-params :adaptive))
	  (verbosity (param init-params :verbosity)))
      
      ;; parameters checking
      (assert (typep (param params :iter) 'fixnum) nil "Number of iterations not integer")
      (when (or (not (typep verbosity 'fixnum))
		(minusp verbosity))
	(setf verbosity 0))
      (when (> verbosity 3) (setf verbosity 3))
      (unless (param params :eps)
	(when (>= verbosity 2) (info "Using default parameter: eps = 0.35~%"))
	(setf (param params :eps) 0.35))
      (unless (param params :mu)
	(when (>= verbosity 2) (info "Using default parameter: mu = 2.0~%"))
	(setf (param params :mu) 2.0))
      (unless recompute-limit
	(when (>= verbosity 2) (info "Using unlimited recomputation limit~%")))
      (unless (not adaptivity) 
	(when (>= verbosity 2) (info "Using adaptive update~%")))
      ;; main loop
      (dotimes (i (param params :iter))
	(incf last-epoch)
	(let* ((eps (/ (param params :eps) num-patterns))
	       (mu (param params :mu))
	       (shrink-factor (/ mu (+ mu 1.0)))
	       (sum-delta-weights (unless adaptivity (w-like))))
	  (setf prev-epoch-err epoch-err
		epoch-err 0)
	  (do-patterns-shuffle (train-patterns p)
	    (eval-layer (first p) weights act-fn :dest out) ;; output in out
	    (copy out lgrad)
	    (m- lgrad (second p)) ;; residual in lgrad
	    (incf epoch-err (square (e-norm lgrad)))
	    (map-matrix out act-fn-deriv) ;; output deriv in out
	    (when deriv-offset (m+c out deriv-offset))
	    (m* lgrad out) ;; local gradient at lgrad
	    (ger lgrad (first p) :dest slopes) ;; slopes
	    
	    (map-three-matrices delta-weights slopes prev-slopes
				#'(lambda (d s ps)
				    (quickprop-update d s ps eps mu shrink-factor)))

	    (if adaptivity
		(m+ weights delta-weights)
		(m+ sum-delta-weights delta-weights))
	    (when decay
	      (map-two-matrices slopes weights
				#'(lambda (s w)
				    (+ s (* w decay)))))
	    (copy slopes prev-slopes)
	    (m- slopes slopes))
	  (unless adaptivity
	    (m+ weights (m/c sum-delta-weights (float num-patterns))))
	  (setf epoch-err (/ epoch-err num-patterns))

	  (when test-patterns
	    (setf prev-test-err test-err
		  test-err (sse-patterns-layer-error test-patterns weights act-fn)))

	  (when (>= verbosity 3)
	    (if test-patterns
		(info "Current error: ~A, over test patterns: ~A~%" epoch-err test-err)
		(info "Current error: ~A~%" epoch-err)))

	  (when recompute-limit
	    (if	(if test-patterns
		    (<= prev-test-err (* (1+ (param params :thr)) test-err))
		    (<= prev-epoch-err (* (1+ (param params :thr)) epoch-err)))
		(decf (param params :recompute))
		(when (< (param params :recompute) recompute-limit)
		  (incf (param params :recompute))))
	    (when (minusp (param params :recompute))
	      (when (>= verbosity 1) (info "Stagnant at ~A epoch~%" i))
	      (return)))))
      (when (>= verbosity 1)
	(if test-patterns
	    (info "Final error: ~A, over test patterns: ~A~%" epoch-err test-err)
	    (info "Final error: ~A~%" epoch-err)))
      (values weights epoch-err last-epoch))))
