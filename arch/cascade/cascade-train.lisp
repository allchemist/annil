(in-package :annil)

(export '(train-cascade-network))
(declaim (inline cascade-compute-out-slopes cascade-compute-out-errors))

(declaim (ftype (function (cascade-network (simple-array single-float) t t)
			  (values (simple-array single-float) single-float))
		cascade-compute-out-slopes))
(defun cascade-compute-out-slopes (cascade slopes patterns params)
  (declare (optimize speed (safety 0))
	   (ignore params))
  (let ((lgrad (the (simple-array single-float) (make-matrix (cascade-noutputs cascade))))
	(err 0.0)
	(act-fn (when (cascade-if-nonlinear-outputs cascade) (cascade-act-fn cascade)))
	(weights-p (patterns-with-weights-p patterns))
	(npats (float (num-patterns patterns)))
	(nouts (patterns-output-dim patterns))
	;(classify-range (param params :misclassify-limit))
	)
    (do-patterns-safe (patterns p i)
      ;; calculate local grad and error
      (scopy (cascade-eval-output cascade (first p)) lgrad)
      (saxpy (second p) lgrad -1.0)
      (%incf err (%square (e-norm lgrad)))
      ;;;; count error bits
      ;; (when classify-range
      ;;   (%dotimes (i (dim0 lgrad))
      ;;     (when (> (abs (%fvref lgrad i)) classify-range)
      ;;       (incf err-bits))))
      ;; calculate slopes
      (when weights-p (sscal lgrad (%* (pattern-weight p) npats)))
      (when act-fn
	(smap-two-matrices-* lgrad (activation-deriv (cascade-outputs cascade) act-fn)))
      (cascade-out-slopes cascade lgrad :values (first p) :dest slopes))
    (values slopes err)))

(defun cascade-compute-out-errors (cascade patterns)
  (declare (optimize speed (safety 0)))
  (let ((err 0.0)
	(weights-p (patterns-with-weights-p patterns))
	(npats (float (num-patterns patterns))))
    (do-patterns-safe (patterns p)
      (let ((out (cascade-eval-output cascade (first p))))
	(m- out (second p))
	(if weights-p
	    (%incf err (%* (%square (%* (pattern-weight p) npats))
			   (%square (e-norm out))))
	    (%incf err (%square (e-norm out))))
	;;;; count error bits
	;; (when classify-range
	;;   (%dotimes (i (dim0 out))
	;;     (when (< (%fvref out i) classify-range)
	;;       (incf err-bits))))))
	))
    err))

(defun cascade-train-output (cascade patterns test-part params)
  (let ((cache (make-array (num-patterns patterns)))
	(method (or (param params :method) :quickprop))
	train test)
    (dotimes (i (num-patterns patterns))
      (let ((p (get-pattern-safe patterns i)))
	(cascade-install-input cascade (first p))
	(cascade-eval-node cascade (1- (cascade-nunits cascade)))
	(setf (svref cache i)
	      (cons (copy (cascade-values cascade))
		    (cdr p)))))
    (if (or (null test-part) (zerop test-part))
	(setf train cache)
	(multiple-value-setq (train test)
	  (cv-split-patterns cache test-part (param params :test-sequential))))
    (flet ((slopes-fn (w s)
	     (declare (ignore w)
		      (type (simple-array single-float) s)
		      (optimize speed (safety 0)))
	     (cascade-compute-out-slopes cascade s train params))
	   (err-fn (w)
	     (declare (ignore w)
		      (optimize speed (safety 0)))
	     (cascade-compute-out-errors cascade (or test train))))
      (optimize-with-restarts (cascade-out-weights cascade)
			      method #'slopes-fn #'err-fn train test params))))

(defun train-cascade-network (cascade patterns test-part nodes params cc-params &optional display-fn)
  (let ((last-epoch 0) (npats (num-patterns patterns))
	(best-crit most-positive-fixnum)
	(neg-err-bits 0) (pos-err-bits 0)
	(max-neg-err-bits (or (param params :max-neg-err-bits) (param params :max-err-bits) 0))
	(max-pos-err-bits (or (param params :max-pos-err-bits) (param params :max-err-bits) 0))
	(classify-range (param params :misclassify-limit))
	(verbosity (or (param params :verbosity) 0)))
    (when classify-range
      (when (floatp max-pos-err-bits)
	(let ((num-pos-patterns 0))
	  (do-patterns-safe (patterns p)
	    (when (plusp (aref (second p) 0)) (incf num-pos-patterns)))
	  (setf max-pos-err-bits (round (* max-pos-err-bits num-pos-patterns)))
	  (setf max-neg-err-bits (round (* max-neg-err-bits (- (num-patterns patterns) num-pos-patterns)))))))
    (flet ((display () (when display-fn (funcall display-fn cascade))))
      (flet ((train-outputs ()
	       (multiple-value-bind (w c e)
		   (cascade-train-output cascade patterns (cascade-out-test-part-size test-part cascade npats) params)
		 (declare (ignore w))
		 (setf best-crit c)
		 (incf last-epoch e))
	       (display)
	       (when classify-range
		 (multiple-value-bind (sum neg-bits pos-bits)
		     (compute-network-err cascade patterns classify-range)
		   (declare (ignore sum))
		   (setf neg-err-bits neg-bits
			 pos-err-bits pos-bits))
		 (when (>= verbosity 2)
		   (info "Error bits count: ~A / ~A~%" neg-err-bits pos-err-bits))
		 (if (and (<= pos-err-bits max-pos-err-bits)
		          (<= neg-err-bits max-neg-err-bits))
		     ;(<= (+ pos-err-bits neg-err-bits) (+ max-pos-err-bits max-neg-err-bits))
		     (progn (when (>= verbosity 1) (info "No other nodes needed~%")) t)
		     nil))))
;;; ==============================================================
	(when (train-outputs) (return-from train-cascade-network cascade))
	(dotimes (i nodes)
	  (when (>= verbosity 1) (info "Training ~A node~%" (1+ i)))
	  (multiple-value-bind (w c e)
	      (cascade-train-hidden-c2 cascade patterns (param cc-params :candidates)
				       nil ;(cascade-cand-test-part-size test-part cascade npats)
				       cc-params)
	    (declare (ignore c e))
	    (cascade-install-hidden-c2 cascade w))
	  (when (train-outputs) (return))
	  (when (>= verbosity 2) (info "Epochs passed: ~A~%" last-epoch))))))
  cascade)