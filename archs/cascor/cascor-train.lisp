(in-package :annil)

;; training output

(defun cascor-train-output (cascor patterns params)
  (assert (cascor-without-unconnected-node-p cascor) nil "There is an unconnected node")
  (let ((full-patterns (cascor-full-patterns cascor patterns)))
    (setf (cascor-output-weights cascor)
	  (quickprop-sse (cascor-output-weights cascor) full-patterns (cascor-act-fn cascor) (param params :iter)  params))))

;; training hidden

;; args -- node-vals: vec[N], net-errors: list(vec[N])[O]
;; return -- float
(defun cascor-corr (node-vals net-errors &key signs-dest)
  (let ((cvals (m-c (copy node-vals) (mean node-vals)))
	(corr 0))
    (dotimes (o (length net-errors))
      (let ((corr-val (inner-prod cvals (elt net-errors o))))
	(when signs-dest
	  (setf (aref signs-dest o) (signum corr-val)))
	(incf corr (abs corr-val))))
    corr))

;; params: cc-epsilon :cc-mu
(defun cascor-train-hidden-quickprop (output-weights full-patterns act-fn iter params)
  (declare (optimize debug))
  (let ((N (num-patterns full-patterns))
	(I (patterns-input-dim full-patterns))
	(O (patterns-output-dim full-patterns)))
    (let ((node-vals (make-matrix N))
	  (mean-val 0.0)
	  (node-weights (make-random-matrix I))
	  (net-errors nil) ;; list(vec[N])[O]
	  (mean-errors (make-matrix O))
	  (tmp-O (make-matrix O))
	  (tmp-I (make-matrix I))
	  (delta-weights (make-matrix I))
	  (slopes (make-matrix I))
	  (prev-slopes (make-matrix I))
	  (corr (- most-negative-fixnum (param params :thr) 1))
	  (prev-corr most-negative-fixnum)
	  (signs (make-matrix O))
	  (sse 0.0)
	  (act-fn-deriv (deriv-fn-name act-fn)))

      ;; cache out errors
      (dotimes (i O)
	(push (make-matrix N) net-errors))
      (dotimes (j N)
	(let ((p (get-pattern full-patterns j)))
	  (map-two-matrices (gemv output-weights (first p) :dest tmp-O)
			    (second p)
			    #'(lambda (o p)
				(let ((val (funcall act-fn o)))
				  (* (- val p)
				     (funcall act-fn-deriv val)))))
	  (dotimes (i O)
	    (setf (aref (elt net-errors i) j) (aref tmp-O i)))))
      (dolist (o net-errors)
	(dotimes (j N)
	  (incf sse (square (aref o j))))
	(m-c o (mean o)))

      ;; adjust
      (dotimes (e iter)
	(let* ((eps (/ (param params :cc-epsilon) N I))
	       (mu (param params :cc-mu))
	       (shrink-factor (/ mu (1+ mu))))
	  ;; calculate node-vals for each pattern
	  (dotimes (j N)
	    (let ((p (get-pattern full-patterns j)))
	      (setf (aref node-vals j) (funcall act-fn (inner-prod (first p) node-weights)))))

	  (setf prev-corr corr
		corr (cascor-corr node-vals net-errors :signs-dest signs))
;	  (info "corr: ~A~%" corr)
	  (when (< (abs (- corr prev-corr)) (param params :cc-thr))
	    (return))
	  
	  (m- slopes slopes)
	  (dotimes (j N)
	    (let ((p (get-pattern full-patterns j)))
	      (let ((lg 0.0))
		(dotimes (i O)
		  (incf lg (* (aref signs i)
			      (aref (elt net-errors i) j)
			      (funcall act-fn-deriv (aref node-vals j)))))
		(m+ slopes (m*c (copy (first p) tmp-I) (/ lg sse))))))
	  
	  (map-three-matrices delta-weights slopes prev-slopes
			      #'(lambda (d s ps)
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
				    step)))
	  
	  (m- node-weights delta-weights)
	  (copy slopes prev-slopes)))
      (info "corr: ~A~%" corr)
      node-weights)))

(defun cascor-train-hidden (cascor patterns params)
  ;; check for unconnected node to be trained
  (assert (not (cascor-without-unconnected-node-p cascor)) nil "No unconnected nodes")
  (let ((full-patterns (cascor-full-patterns cascor patterns))
	(node (1- (cascor-hidden-num cascor))))
    (setf (cascor-node-weights cascor node)
	  (cascor-train-hidden-quickprop (cascor-output-weights cascor) full-patterns (cascor-act-fn cascor) (param params :cc-iter) (copy-tree params)))))

(defun cascor-train (cascor patterns nodes params)
  (cascor-train-output cascor patterns params)
  (dotimes (n nodes)
    (info "Training ~A node~%"
	  (cascor-insert-node cascor))
    (cascor-train-hidden cascor patterns params)
    (info "Connecting to outputs~%")
    (cascor-connect-node cascor)
    (cascor-train-output cascor patterns params))
  cascor)

(defmethod eval-network ((network cascor) input)
  (cascor-eval-output network input))
