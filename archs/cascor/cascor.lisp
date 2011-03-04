(in-package :annil)

(defclass cascor (network)
  ((hidden-weights :initarg :hidden-weights :accessor cascor-hidden-weights)
   (output-weights :initarg :output-weights :accessor cascor-output-weights)
   (input-size     :initarg :input-size     :accessor cascor-input-size)
   (full-input     :initarg :full-input     :accessor cascor-full-input)
   (output-nodes   :initarg :output-nodes   :accessor cascor-output-nodes)
   (act-fn         :initarg :act-fn         :accessor cascor-act-fn)))

(defun make-cascor (input-size output-size act-fn)
  (make-instance 'cascor
		 :hidden-weights nil
		 :output-weights (make-matrix (list output-size (1+ input-size)))
		 :input-size input-size
		 :full-input (make-matrix (1+ input-size))
		 :output-nodes (make-matrix output-size)
		 :act-fn act-fn))

(defun make-random-cascor (input-size output-size act-fn &optional nw-init)
  (let ((cascor (make-cascor input-size output-size act-fn)))
    ;; set random output weights
    (m*c (map-matrix (cascor-output-weights cascor) 'simple-rng) 0.1)
    ;; apply Nguyen-Widrow rule if needed
    ;; nw-init must be input ranges (a list of two)
    (when nw-init (initialize-weights (cascor-output-weights cascor) nw-init))
    ;; setf bias (next to inputs in full-input)
    (setf (aref (cascor-full-input cascor) (cascor-input-size cascor)) 1.0)
    cascor))

(defun cascor-hidden-num (cascor)
  (length (cascor-hidden-weights cascor)))

(defun cascor-output-size (cascor)
  (length (cascor-output-nodes cascor)))

(defun cascor-hidden-node-value (cascor node)
  ;; hidden node is next to inputs and bias in full-input
  (aref (cascor-full-input cascor)
	(+ (cascor-input-size cascor) 1 node)))

(defun cascor-node-weights (cascor node)
  (elt (cascor-hidden-weights cascor) node))

(defun (setf cascor-node-weights) (weights cascor node)
  (setf (elt (cascor-hidden-weights cascor) node) weights))

(defun cascor-without-unconnected-node-p (cascor)
  ;; compare hidden-num and weights from hidden nodes to one output node
  ;; equal when no unconnected node
  (= (- (dim1 (cascor-output-weights cascor))
	(cascor-input-size cascor)
	1)
     (cascor-hidden-num cascor)))

(defmethod print-object ((cascor cascor) stream)
  (format stream "#<cascade correlation network with~%~A input, ~A hidden, ~A output nodes, ~A unconnected node,~%~A activation function>"
	  (cascor-input-size cascor) (cascor-hidden-num cascor) (cascor-output-size cascor)
	  (if (cascor-without-unconnected-node-p cascor) "without" "with")
	  (case (cascor-act-fn cascor)
	    (linear-fn "linear")
	    (logistic-fn "logistic")
	    (tanh-fn "hyperbolyc tangent")
	    (t "unknown"))))

(defun cascor-insert-node (cascor)
  (let* ((full-input (cascor-full-input cascor))
	 (len (length full-input)))
    ;; check if there is an unconnected node
    (assert (cascor-without-unconnected-node-p cascor) nil "An unconnected node already exists")
    ;; till this node and output are unconnected,
    ;; only add weights for incoming connections for this node
    (setf (cascor-hidden-weights cascor)
	  (nconc (cascor-hidden-weights cascor)
		 (list (m*c (make-random-matrix len) 0.1))))
    ;; and return the index of the node (0 for first hidden)
    (- len (cascor-input-size cascor) 1)))

(defun cascor-connect-node (cascor)
  (let* ((output-weights (cascor-output-weights cascor))
	 (full-input (cascor-full-input cascor))
	 (dim0 (dim0 output-weights))
	 (dim1 (dim1 output-weights)))
    ;; check if there are any unconnected nodes
    (assert (not (cascor-without-unconnected-node-p cascor)) nil "No unconnected nodes")
    ;; add connections to output nodes
    ;; kludge for adjusting not-adjustable array
    (setf (cascor-output-weights cascor) (adjust-array output-weights (list dim0 (1+ dim1))))
    (setf output-weights (cascor-output-weights cascor))
    (dotimes (i dim0)
      (setf (aref output-weights i dim1) (* (simple-rng 1.0) 0.1)))
    ;; full-input now has one more node
    (setf (cascor-full-input cascor) (adjust-array full-input (1+ (dim0 full-input))))
    cascor))

(defun cascor-eval-node (cascor full-input node)
  (funcall (cascor-act-fn cascor)
	   ;; full-input can be larger, than weights for some nodes
	   ;; note, that 'inner-prod' takes only necessary part of it
	   (inner-prod full-input (elt (cascor-hidden-weights cascor) node))))

(defun eval-cascor-full-input (cascor input)
  (let ((full-input (cascor-full-input cascor))
	(first-hidden-pos (1+ (cascor-input-size cascor))))
    ;; insert input values into full-input
    (copy input full-input)
    ;; for every hidden node (if any, except unconnected)
    (dotimes (i (let ((num (cascor-hidden-num cascor)))
		  (when (not (cascor-without-unconnected-node-p cascor)) (decf num))
		  num))
      ;; set node value into full-input
      (setf (aref full-input (+ first-hidden-pos i))
	    ;; using current full-input with values of previous hidden nodes
	    (cascor-eval-node cascor full-input i)))
    full-input))

(defun cascor-eval-output (cascor input)
  (map-matrix
   (gemv (cascor-output-weights cascor)
	 (eval-cascor-full-input cascor input))
   (cascor-act-fn cascor)))

(defmethod eval-network ((network cascor) input)
  (cascor-eval-output network input))

;; KLUDGE

(defun cascor-full-patterns (cascor patterns)
  (let ((full-patterns nil))
    (do-patterns (patterns p)
      (push (list (copy (eval-cascor-full-input cascor (first p))) (second p)) full-patterns))
    (nreverse full-patterns)))
