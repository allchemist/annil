(in-package :annil)

(defun rprop-update (w d pd pdw s ps dmin dmax err err-1)
  (dotimes (i (array-total-size w))
    (declare (type fixnum i))
    (let ((sv (row-major-aref s i))
	  (psv (row-major-aref ps i)))
      (cond ((plusp (* sv psv)); (< err err-1))
	     ;; s*ps>0 =>
	     ;; d=min(pd*n+,dmax)
	     ;; w+=-sign(s)*d
	     ;; ps=s
	     (incf (row-major-aref w i)
		   (setf (row-major-aref pdw i)
			 (- (* (signum sv)
			       (setf (row-major-aref d i)
				     (min (* (row-major-aref pd i) 1.2) dmax))))))
	     (setf (row-major-aref ps i) sv))
	    ((and (minusp (* sv psv)) (> err err-1))
	     ;; s*ps<0 =>
	     ;; d=max(pd*n-,dmin)
	     ;; ps=0
	     (setf (row-major-aref d i)
		   (max (* (row-major-aref pd i) 0.5) dmin))
	     (when (> err err-1)
	       (decf (row-major-aref w i)
		     (row-major-aref pdw i)))
	     (setf (row-major-aref ps i) 0.0))
	    ((zerop (* sv psv))
	     ;; s*ps=0 =>
	     ;; w+=-sign(s)*d
	     ;; ps=s
	     (incf (row-major-aref w i)
		   (setf (row-major-aref pdw i)
			 (- (* (signum sv) (row-major-aref d i)))))
	     (setf (row-major-aref ps i) sv))))))

(defun rprop (weights slopes-fn err-fn train-patterns test-patterns init-params)
  (flet ((w-like () (make-matrix-like weights)))
    (let ((large-val (float most-positive-fixnum)))
      (let ((slopes (w-like))
	    (prev-slopes (w-like))
	    (deltas (make-matrix (array-dimensions weights) :initial-element (param init-params :d0)))
	    (prev-deltas (w-like))
	    (prev-delta-weights (w-like))
	    (best-weights (w-like))

	    (npats (num-patterns train-patterns))
	    (test-npats (num-patterns test-patterns))
	    (err large-val) (err-1 0.0)
	    (test-err large-val) (test-err-1 0.0)
	    (best-crit large-val)
	    (epochs-passed 0) (recompute-flag nil)

	    (params (copy-tree init-params))
	    (dmin (param init-params :dmin))
	    (dmax (param init-params :dmax))
	    (thr (or (param init-params :thr) 0.0))
	    (verbosity (param init-params :verbosity))
	    (recompute-limit (param init-params :recompute)))
	    ;; main loop
	(symbol-macrolet
	    ((crit (if test-patterns test-err err))
	     (crit-1 (if test-patterns test-err-1 err-1)))
	  (dotimes (i (param params :epochs))
	    (incf epochs-passed)
;;; ==============================================================
	    ;; optimization part
	    (multiple-value-bind (s e)
		(funcall slopes-fn weights slopes)
	      (declare (ignore s))
	      (setf err-1 err
		    err (/ e npats)))
	    (when (and (not test-patterns)
		       (< crit best-crit))
	      (setf best-crit crit)
	      (copy weights best-weights))
	    (rprop-update weights deltas prev-deltas prev-delta-weights slopes prev-slopes
			  dmax dmin err err-1)
	    (copy deltas prev-deltas)
	    (when test-patterns
	      (setf test-err-1 test-err
		    test-err (/ (funcall err-fn weights) test-npats))
	      (when (< crit best-crit)
		(setf best-crit crit)
		(copy weights best-weights)))
;;; ==============================================================
	    ;; information part
	    (when (>= verbosity 3)
	      (if test-patterns
		  (info "Current result: ~,5F, over test set: ~,5F~%" err test-err)
		  (info "Current result: ~,5F~%" err)))
	    (when recompute-limit
	      (when (<= crit-1 crit) (setf recompute-flag t))
	      (when recompute-flag
		(if (<= crit-1 (* crit (1+ thr)))
		    (decf (param params :recompute))
		    (when (< (param params :recompute) recompute-limit)
		      (incf (param params :recompute) 1/2))))
	      (when (minusp (param params :recompute))
		(when (>= verbosity 2) (info "Stagnant at ~A epoch~%" i))
		(return)))))
	(when (>= verbosity 1) (info "Final result: ~A~%" best-crit))
	(values best-weights best-crit epochs-passed)))))
