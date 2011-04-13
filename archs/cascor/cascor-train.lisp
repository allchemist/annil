(in-package :annil)

(export '(cascor-train cascor-train-output cascor-train-hidden))

(defun cascor-train (cascor patterns test-part nodes params cc-params)
  (let ((last-epoch 0))
    (incf last-epoch (nth-value 2 (cascor-train-output cascor patterns test-part params)))
    (dotimes (i nodes)
      (info "~%")
      (info "Training ~A node~%" (1+ (cascor-insert-node cascor)))
      (cascor-train-hidden cascor patterns test-part cc-params)
      (cascor-connect-node cascor)
      (info "Connecting to outputs~%")
      (incf last-epoch (nth-value 2 (cascor-train-output cascor patterns test-part params)))
      (info "Epochs passed: ~A~%" last-epoch)))
  cascor)

(defun cascor-train-output (cascor patterns test-part params)
  ;; perform sse optimization
  (assert (cascor-without-unconnected-node-p cascor) nil "There is an unconnected node")
  (optimize-sse (cascor-output-weights cascor)
		(cascor-full-patterns cascor patterns)
		test-part (cascor-act-fn cascor) params))

(defun cascor-train-hidden (cascor patterns test-part params)
  ;; check for unconnected node to be trained
  (assert (not (cascor-without-unconnected-node-p cascor)) nil "No unconnected nodes")
  (let ((full-patterns (cascor-full-patterns cascor patterns))
	(node (1- (cascor-hidden-num cascor))))
    (setf (cascor-node-weights cascor node)
	  (cascor-train-hidden-unit (cascor-output-weights cascor) full-patterns test-part
				    (cascor-act-fn cascor) (copy-tree params)))))

(defun cascor-corr (V E &optional sign-dest)
  (let ((V-c (m-c (copy V) (mean V)))
	(corr 0))
    (dotimes (o (length E))
      (let ((corr-val (inner-prod V-c (svref E o))))
	(when sign-dest
	  (setf (aref sign-dest o) (signum corr-val)))
	(incf corr (abs corr-val))))
    corr))

(defun cascor-train-hidden-unit (output-weights full-patterns test-part act-fn params)
  (let ((-N (num-patterns full-patterns))
	(-I (patterns-input-dim full-patterns))
	(-O (patterns-output-dim full-patterns))
	(act-fn-deriv (deriv-fn-name act-fn))
	(method (or (param params :method) :quickprop))
	(verbosity (param params :verbosity)))
    (let ((V (make-matrix -N))
	  (E (coerce (loop for i from 0 below -O collect (make-matrix -N)) 'simple-vector))
	  (sse 0.0))
      ;; caching output errors
      (let ((tmp-out (make-matrix -O)))
	(dotimes (j -N)
	  (let ((p (get-pattern-safe full-patterns j)))
	    (map-two-matrices (gemv output-weights (first p) :dest tmp-out)
			      (second p)
			      #'(lambda (o d)
				  (let ((val (funcall act-fn o)))
				    (* (- val d)
				       (funcall act-fn-deriv val))))))
	  (dotimes (i -O)
	    (setf (aref (svref E i) j) (aref tmp-out i))))
	(dotimes (i -O)
	  (let ((o (svref E i)))
	    (dotimes (j -N)
	      (incf sse (square (aref o j))))
	    (m-c o (mean o)))))
      ;; slopes calculating
      (let ((sign-dest (make-matrix -O)))
	(flet ((cc-compute-slopes (w s)
		 (dotimes (j -N)
		   (let ((p (get-pattern-safe full-patterns j)))
		     (setf (aref V j) (funcall act-fn (inner-prod (first p) w)))))
		 (let ((corr (cascor-corr V E sign-dest)))
		   (dotimes (j -N)
		     (let ((p (get-pattern-safe full-patterns j))
			   (lg 0.0))
		       (dotimes (i -O)
			 (incf lg (* (aref sign-dest i)
				     (aref (svref E i) j)
				     (funcall act-fn-deriv (aref V j)))))
		       (axpy (first p) s (/ lg sse))))
		   (when (eq method :quickprop) (m*c s -1.0))
		   (values s (- (* corr -N))))))
	  (when (>= verbosity 1) (info "Training candidates "))
	  (let (candidates)
	    (decf (param params :verbosity) 2)
	    (dotimes (i (param params :candidates))
	      (when (= verbosity 2) (princ "."))
	      (when (>= verbosity 3) (terpri) (info "New candidate~%"))
	      (multiple-value-bind (cand-weights cand-corr) 
		  (funcall (intern (string method) :annil)
			   (m-c (make-random-matrix -I) 0.5)
			   #'cc-compute-slopes nil full-patterns nil params)
		(push (cons cand-corr cand-weights) candidates)))
	    (let ((best (first (sort (mapcar #'car candidates) #'<))))
	      (when (= verbosity 2) (terpri))
	      (when (>= verbosity 2)
		(info "Per-candidate corr: ~A~%" (mapcar #'- (mapcar #'car candidates))))
	      (when (>= verbosity 1)
		(info "Best corr: ~A~%" (- best)))
	      (cdr (find best candidates :key #'car)))))))))
