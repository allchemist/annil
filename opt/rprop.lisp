(in-package :annil)

(export '(rprop rprop-single))

#|
;; original version
(defun rprop (w slope-fn dmax dmin d0 epochs decay)
  ;; init
  ;; {ps}=0, {d}=d0
  (let ((s (make-matrix-like w))
	(ps (make-matrix-like w))
	(d (make-matrix (array-dimensions w) :initial-element d0))
	(pd (make-matrix-like w)))
    ;; repeat till converge
    (dotimes (e epochs)
      ;; eval slopes
      (funcall slope-fn w s)
      ;; for each weight
      (dotimes (i (array-total-size w))
	(let ((sv (row-major-aref s i))
	      (psv (row-major-aref ps i)))
	  (cond ((plusp (* sv psv))
		   ;; s*ps>0 =>
		   ;; d=min(pd*n+,dmax)
		   ;; w+=-sign(s)*d
		   ;; ps=s
		   (incf (row-major-aref w i)
			 (- (* (signum sv)
			       (setf (row-major-aref d i)
				     (min (* (row-major-aref pd i) 1.2) dmax)))))
		   (setf (row-major-aref ps i) sv))
		  ((minusp (* sv psv))
		   ;; s*ps<0 =>
		   ;; d=max(pd*n-,dmin)
		   ;; ps=0
		   (setf (row-major-aref d i)
			 (max (* (row-major-aref pd i) 0.5) dmin))
		   (setf (row-major-aref ps i) 0.0))
		  ((zerop (* sv psv))
		   ;; s*ps=0 =>
		   ;; w+=-sign(s)*d
		   ;; ps=s
		   (incf (row-major-aref w i)
			 (- (* (signum sv) (row-major-aref d i))))
		   (setf (row-major-aref ps i) sv)))))
      (when decay (m*c w (- 1.0 decay)))
      (copy d pd)))
  w)
|#

(defun rprop (w slope-fn err-fn train-patterns test-patterns params)
  (let ((restarts (or (param params :restarts) 0))
	(err most-positive-fixnum)
	(epochs 0)
	(copy-params (copy-tree params)))
    (decf (param copy-params :verbosity))
    (dotimes (i (1+ restarts))
      (multiple-value-bind (cur-w cur-err cur-epochs)
	  (rprop-single w slope-fn err-fn train-patterns test-patterns copy-params)
	(setf (param copy-params :d0) (/ (param copy-params :d0) 5))
	(setf (param copy-params :dmin) (/ (param copy-params :dmin) 5))
	(setf (param copy-params :dmax) (/ (param copy-params :dmax) 2))
	(setf (param copy-params :thr) (/ (param copy-params :thr) 5))
	(setf err cur-err)
	(incf epochs cur-epochs)))
    (incf (param copy-params :verbosity))
    (values w err epochs)))

(defun rprop-single (w slope-fn err-fn train-patterns test-patterns init-params)
  ;; init
  ;; {ps}=0, {d}=d0
  (let ((s (make-matrix-like w))
	(ps (make-matrix-like w))
	(d (make-matrix (array-dimensions w) :initial-element (param init-params :d0)))
	(pd (make-matrix-like w))
	(pdw (make-matrix-like w))
	(err-1 0.0)
	(err (coerce most-positive-fixnum 'single-float))
	(test-err-1 0.0)
	(test-err (coerce most-positive-fixnum 'single-float))
	(epochs-passed 0)
	;; params
	(dmin (param init-params :dmin))
	(dmax (param init-params :dmax))
	(thr (or (param init-params :thr) 0.0))
	(verbosity (param init-params :verbosity))
	(decay (param init-params :decay))
	(params (copy-tree init-params))
	(recompute-limit (param init-params :recompute))
	(recompute-flag nil)
	(best-weights (make-matrix-like w))
	(best-err (float most-positive-fixnum))
	(num-patterns (num-patterns train-patterns))
	(test-num-patterns (when test-patterns (num-patterns test-patterns))))
    ;; parameter checking
    (assert (typep (param params :epochs) 'fixnum) nil "Number of iterations not integer")
      (when (or (not (typep verbosity 'fixnum))
		(minusp verbosity))
	(setf verbosity 0))
    (when (> verbosity 3) (setf verbosity 3))
    (unless (param params :dmax)
      (when (>= verbosity 3) (info "Using default parameter: dmax = 50.0~%"))
      (setf (param params :dmax) 50.0))
    (unless (param params :dmax)
      (when (>= verbosity 3) (info "Using default parameter: dmin = 1.e-6~%"))
      (setf (param params :dmin) 1.e-6))
    (unless (param params :d0)
      (when (>= verbosity 3) (info "Using default parameter: d0 = 0.01~%"))
      (setf (param params :d0) 0.01))
    (unless recompute-limit
      (when (>= verbosity 3) (info "Using unlimited recomputation limit~%")))
    ;; repeat till converge
    (dotimes (e (param params :epochs))
;;; =================================================================
      (incf epochs-passed)
      ;; eval slopes
      (setf err-1 err
	    err (/ (nth-value 1 (funcall slope-fn w s)) num-patterns))
      (m/c s (* err num-patterns))
      (when (< (if test-patterns test-err err) best-err)
	(setf best-err (if test-patterns test-err err))
	(copy w best-weights))
      ;; for each weight
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
					   (min (* (row-major-aref pd i) 1.3) dmax))))))
		   (setf (row-major-aref ps i) sv))
		  ((and (minusp (* sv psv)) (> err err-1))
		   ;; s*ps<0 =>
		   ;; d=max(pd*n-,dmin)
		   ;; ps=0
		   (setf (row-major-aref d i)
			 (max (* (row-major-aref pd i) 0.6) dmin))
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
		   (setf (row-major-aref ps i) sv)))))
      (when decay (m*c w (- 1.0 decay)))
      (copy d pd)
;;; =================================================================
      ;; afterall checks
      (when test-patterns
	    (setf test-err-1 test-err
		  test-err (/ (funcall err-fn w) test-num-patterns)))      
      (when (>= verbosity 3)
	(if test-patterns
	    (format *query-io* " ;; Current result: ~A, over test patterns: ~A~%" err test-err)
	    (format *query-io* " ;; Current result: ~A~%" err)))
      (when recompute-limit
	(when (if test-patterns
		  (<= test-err-1 test-err)
		  (<= err-1 err))
	  (setf recompute-flag t))
	(if (if test-patterns
		(<= test-err-1 (* (1+ thr) test-err))
		(<= err-1 (* (1+ thr) err)))
	    (when recompute-flag (decf (param params :recompute)))
	    (when recompute-flag
	      (when (< (param params :recompute) recompute-limit)
		(incf (param params :recompute) 1/2))))
	(when (minusp (param params :recompute))
	  (when (>= verbosity 2) (info "Stagnant at ~A epoch~%" e))
	  (return))))
    (when (>= verbosity 1)
      (if test-patterns
	  (info "Final result: ~A, over test patterns: ~A~%" best-err test-err)
	  (info "Final result: ~A~%" best-err)))
    (values best-weights best-err epochs-passed)))
