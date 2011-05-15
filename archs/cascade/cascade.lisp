(in-package :annil)

(export '(cascade-network cascade-act-fn cascade-if-nonlinear-outputs cascade-ninputs cascade-noutputs cascade-nhidden
	  cascade-conns cascade-values cascade-weights cascade-outputs cascade-out-conns cascade-out-weights
	  make-cascade-network cascade-nunits cascade-insert-node cascade-install-input cascade-node-conns
	  cascade-node-weights cascade-node-value cascade-out-node-weights cascade-eval-node cascade-eval-output
	  cascade-eval cascade-num-weights))

(declaim (inline cascade-if-nonlinear-outputs cascade-ninputs cascade-noutputs cascade-nhidden
		 cascade-conns cascade-values cascade-weights cascade-outputs cascade-out-conns
		 cascade-out-weights cascade-nunits cascade-node-conns cascade-node-weights
		 cascade-node-value cascade-out-node-weights cascade-eval-node))

(defclass cascade-network (network)
  ((act-fn :initarg :act-fn :accessor cascade-act-fn)
   (if-nonlinear-outputs :initarg :if-nonlinear-outputs :accessor cascade-if-nonlinear-outputs)
   (ninputs :initarg :ninputs :accessor cascade-ninputs :type fixnum)
   (noutputs :initarg :noutputs :accessor cascade-noutputs :type fixnum)
   (nhidden :initarg :nhidden :accessor cascade-nhidden :type fixnum)
   (conns :initarg :conns :accessor cascade-conns :type simple-vector)
   (values :initarg :values :accessor cascade-values :type (simple-array single-float (*)))
   (weights :initarg :weights :accessor cascade-weights :type simple-vector)
   (outputs :initarg :outputs :accessor cascade-outputs :type (simple-array single-float (*)))
   (out-conns :initarg :out-conns :accessor cascade-out-conns :type simple-vector)
   (out-weights :initarg :out-weights :accessor cascade-out-weights :type (simple-array single-float (*)))
   ))

(defun make-cascade-network (input-size output-size act-fn
			     &optional (if-nonlinear-outputs t))
  (let ((cascade
	 (make-instance 'cascade-network
			:act-fn act-fn
			:if-nonlinear-outputs if-nonlinear-outputs
			:ninputs input-size
			:noutputs output-size
			:nhidden 0
			:values (make-matrix (1+ input-size))
			:conns (make-array (1+ input-size) :initial-element nil)
			:weights (make-array (1+ input-size) :initial-element nil)
			:outputs (make-matrix output-size)
			:out-conns (make-array output-size :initial-element nil)
			:out-weights (seed-weights (make-matrix (* output-size (1+ input-size)))))))
    (setf (aref (cascade-values cascade) input-size) 1.0)
    (dotimes (i output-size)
      (setf (svref (cascade-out-conns cascade) i) (iota (1+ input-size))))
    cascade))

(defun cascade-nunits (cascade)
  (+ (cascade-ninputs cascade) (cascade-nhidden cascade) 1))

(defun cascade-insert-node (cascade &optional cand-conns)
  (symbol-macrolet ((w (cascade-weights cascade))
		    (v (cascade-values cascade))
		    (c (cascade-conns cascade))
		    (ow (cascade-out-weights cascade))
		    (ov (cascade-outputs cascade))
		    (oc (cascade-out-conns cascade)))
    (let ((idx (cascade-nunits cascade))
	  (nouts (cascade-noutputs cascade)))
      (extend-vector w) (extend-vector v) (extend-vector c)

      (setf (svref c idx) (or cand-conns (iota idx))
	    (aref v idx) 0.0
	    (svref w idx) (seed-weights (make-matrix idx)))
      (incf (cascade-nhidden cascade))
      
      (let ((new-ow (make-matrix (+ (length ow) nouts)))
	    (off 0))
	(dotimes (o nouts)
	  (let ((conns (dim0 (svref oc o))))
	    (copy-with-offset (subseq ow off (+ off conns)) new-ow (+ off o))
	    (setf (aref new-ow (+ off conns o)) (if (zerop (length ow)) (random-value 1.0) (random-elt ow)))
	    (incf off conns)))
	(setf ow new-ow))
      (dotimes (o nouts)
	(setf (aref (extend-vector (svref oc o)) (1- (length (svref oc o)))) idx)))
    cascade))

(defun cascade-install-input (cascade input)
  (copy input (cascade-values cascade)))

(defun cascade-node-conns (cascade node)
  (%svref (cascade-conns cascade) node))

#|(defun cascade-cand-slopes (cascade cand conns lgrad &key values dest)
  (when (not values) (setf values (cascade-values cascade)))
    (if dest
	(assert (= (dim0 dest) (dim0 conns))
		nil "Improper slopes dest dimensions")
	(setf dest (make-matrix (dim0 conns))))
  (%dotimes (i (dim0 conns))
    (%incf (%fvref dest i)
	   (%* lgrad (%fvref values (%ivref conns i)))))
  dest)|#

(defun cascade-out-slopes (cascade lgrad &key values dest)
  (let* ((out-conns (cascade-out-conns cascade))
	 (off 0)
	 (dim0 (apply #'+ (map 'list #'dim0 out-conns)))
	 (dim1 (dim0 lgrad)))
    (when (not values) (setf values (cascade-values cascade)))
    (if dest
	(assert (and (= (dim1 dest) dim0)
		     (= (dim0 dest) dim1))
		nil "Improper slopes dest dimensions")
	(setf dest (make-matrix `(,dim0 ,dim1))))
    (%dotimes (i (cascade-noutputs cascade))
      (let ((conns (%svref out-conns i))
	    (v (%fvref lgrad i)))
	(%dotimes (c (dim0 conns))
	  (%incf (aref dest (+ c off) i)
		 (%* v (%fvref values (%ivref conns c)))))		   
	(incf off (dim0 conns))))
    dest))
  

(defun cascade-node-weights (cascade node)
  (%svref (cascade-weights cascade) node))

(defun cascade-num-weights (cascor)
  (apply #'+
	 (dim0 (cascade-out-weights cascor))
	 (map 'list #'dim0 (remove nil (cascade-weights cascor)))))

(defun cascade-node-value (cascade node)
  (%fvref (cascade-values cascade) node))

(defun cascade-out-node-weights (cascade output)
  (let* ((conns (cascade-out-conns cascade))
	 (start 0))
    (%dotimes (i output)
      (incf start (dim0 (svref conns i))))
    (subseq (cascade-out-weights cascade)
	    start (+ start (dim0 (%svref conns output))))))

(defun cascade-eval-node (cascade node)
  (if (< node (1+ (cascade-ninputs cascade)))
      (cascade-node-value cascade node)
      (let ((act-fn (cascade-act-fn cascade)))
	(loop for n fixnum from (1+ (cascade-ninputs cascade)) to node do
	  (let ((conns (cascade-node-conns cascade n))
		(weights (cascade-node-weights cascade n))
		(values (cascade-values cascade))
		(val 0.0))
	    (%dotimes (i (length conns))
	      (%incf val (%* (%fvref weights i) (%fvref values (%ivref conns i)))))
	    (%setf (%fvref (cascade-values cascade) n) (funcall act-fn val))))
	(cascade-node-value cascade node))))

(defun cascade-eval-output (cascade &optional values)
  (let ((out-weights (cascade-out-weights cascade))
	(out-conns (cascade-out-conns cascade))
	(off 0)
	(act-fn (when (cascade-if-nonlinear-outputs cascade)
		  (cascade-act-fn cascade))))
    (when (not values) (setf values (cascade-values cascade)))
    (%dotimes (i (cascade-noutputs cascade))
      (let ((conns (%svref out-conns i))
	    (val 0.0))
	(%dotimes (c (length conns))
	  (%incf val (%* (%fvref out-weights (+ c off))
			 (%fvref values (%ivref conns c)))))
	(%setf (%fvref (cascade-outputs cascade) i)
	       (if act-fn (funcall act-fn val) val))
	(incf off (dim0 conns)))))
  (cascade-outputs cascade))

(defun cascade-eval (cascade input)
  (cascade-install-input cascade input)
  (cascade-eval-node cascade (1- (cascade-nunits cascade)))
  (cascade-eval-output cascade))

(defmethod eval-network ((network cascade-network) input)
  (cascade-eval network input))

;; pruning

(defun cascade-prune-node-connection (cascade input node)
  (let* ((conns (svref (cascade-conns cascade) node))
	 (idx (position input conns :test #'eq)))
    (assert idx nil "No such connection to prune")
    (let ((new-conns (make-matrix (1- (length conns)) :element-type 'fixnum))
	  (weights (svref (cascade-weights cascade) node))
	  (new-weights (make-matrix (1- (length conns)))))
      (dotimes (i (1- (length conns)))
	(let ((j (if (>= i idx) (1+ i) i)))
	  (setf (aref new-conns i)
		(aref conns j))
	  (setf (aref new-weights i)
		(aref weights j))))
      (setf (svref (cascade-conns cascade) node) new-conns
	    (svref (cascade-weights cascade) node) new-weights)))
  cascade)

(defun cascade-prune-cand-connection (cascade input cand conns)
  (let* ((idx (position input conns :test #'eq)))
    (assert idx nil "No such connection to prune")
    (let ((new-conns (make-matrix (1- (dim0 conns)) :element-type 'fixnum))
	  (new-cand (make-matrix (1- (dim0 conns)))))
      (dotimes (i (1- (dim0 conns)))
	(let ((j (if (>= i idx) (1+ i) i)))
	  (setf (aref new-conns i)
		(aref conns j))
	  (setf (aref new-cand i)
		(aref cand j))))
      (values new-cand new-conns))))

(defun cascade-prune-out-connection (cascade input output)
  (let* ((out-conns (cascade-out-conns cascade))
	 (conns (svref out-conns output))
	 (idx (position input conns :test #'eq)))
    (assert idx nil "No such connection to prune")
    (let* ((new-conns (make-matrix (1- (length conns)) :element-type 'fixnum))
	   (weights (cascade-out-weights cascade))
	   (new-weights (make-matrix (1- (length weights))))
	   (off 0))
      (dotimes (c output)
	(let ((ow (cascade-out-node-weights cascade c)))
	  (copy-with-offset ow new-weights off)
	  (incf off (dim0 ow))))
      (dotimes (i (1- (length conns)))
	(let ((j (if (>= i idx) (1+ i) i)))
	  (setf (aref new-conns i)
		(aref conns j))
	  (setf (aref new-weights (+ off i))
		(aref weights (+ off j)))))
      (incf off (1- (length conns)))
      (loop for c from (1+ output) below (cascade-noutputs cascade) do
	(let ((ow (cascade-out-node-weights cascade c)))
	  (copy-with-offset ow new-weights off)
	  (incf off (dim0 ow))))
      (setf (svref (cascade-out-conns cascade) output) new-conns
	    (cascade-out-weights cascade) new-weights)))
  cascade)
