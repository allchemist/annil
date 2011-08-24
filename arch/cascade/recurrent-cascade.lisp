(in-package :annil)

(export '(recurrent-cascade-network cascade-prev-values cascade-feedbacks
	  make-recurrent-cascade-network train-recurrent-cascade-network))

(defclass recurrent-cascade-network (cascade-network)
  ((prev-values :initarg :prev-values :accessor cascade-prev-values :type sf-seq)
   (feedbacks :initarg :feedbacks :accessor cascade-feedbacks :type sf-seq)))

(defun make-recurrent-cascade-network (input-size output-size act-fn
				       &optional (if-nonlinear-outputs t))
  (change-class (make-cascade-network input-size output-size act-fn if-nonlinear-outputs)
		'recurrent-cascade-network
		:prev-values (make-matrix 0)
		:feedbacks (make-matrix 0)))

(defun rcascade-insert-node (cascade &optional cand-conns)
  (cascade-insert-node cascade cand-conns)
  (extend-vector (cascade-prev-values cascade))
  (extend-vector (cascade-feedbacks cascade))
  cascade)

(defun rcascade-set-prev-values (cascade)
  (let ((prev-values (cascade-prev-values cascade))
	(ninputs (cascade-ninputs cascade))
	(values (cascade-values cascade)))
    (dotimes (i (cascade-nhidden cascade))
      (setf (%fvref prev-values i) (%fvref values (+ i ninputs 1))))
    cascade))

(defun rcascade-eval-node (cascade node)
  (declare (optimize speed (safety 0)))
  (if (< node (1+ (cascade-ninputs cascade)))
      (cascade-node-value cascade node)
      (let ((act-fn (cascade-act-fn cascade))
	    (ninputs (1+ (cascade-ninputs cascade)))
	    (values (cascade-values cascade))
	    (prev-values (cascade-prev-values cascade))
	    (feedbacks (cascade-feedbacks cascade)))
	(loop for n fixnum from ninputs to node do
	  (let ((conns (cascade-node-conns cascade n))
		(weights (cascade-node-weights cascade n))
		(val 0.0))
	    (%dotimes (i (length conns))
	      (%incf val (%* (%fvref weights i)
			     (%fvref values (%ivref conns i)))))
	    (%incf val (%* (%fvref prev-values (- n ninputs)) ; !
			   (%fvref feedbacks (- n ninputs)))) ; !
	    (%setf (%fvref values n)
		   (activation val act-fn))))
	(cascade-node-value cascade node))))

(defun rcascade-eval (cascade input)
  (cascade-install-input cascade input)
  (rcascade-set-prev-values cascade)
  (rcascade-eval-node cascade (1- (cascade-nunits cascade)))
  (cascade-eval-output cascade))

(defmethod eval-network ((network recurrent-cascade-network) input)
  (rcascade-eval network input))
