(in-package :annil)

;; training output

(defun cascor-train-output (cascor train-patterns test-patterns params)
  (assert (cascor-without-unconnected-node-p cascor) nil "There is an unconnected node")
  (let ((full-train-patterns (cascor-full-patterns cascor train-patterns))
	(full-test-patterns  (cascor-full-patterns cascor test-patterns)))
    (setf (cascor-output-weights cascor)
	  (quickprop-sse (cascor-output-weights cascor) full-train-patterns full-test-patterns
			 (cascor-act-fn cascor) params))))

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

;; params: cc-eps :cc-mu, :cc-thr, :cc-iter
(defun cascor-train-hidden-quickprop (output-weights full-patterns act-fn params)
  (let ((N (num-patterns full-patterns))
	(I (patterns-input-dim full-patterns))
	(O (patterns-output-dim full-patterns)))
    (let ((node-vals (make-matrix N))
	  (node-weights (map-matrix (make-matrix I) #'(lambda (x) (plain-rng -0.25 0.25))))
	  (mean-val 0.0)
	  (net-errors nil) ;; list(vec[N])[O]
	  (mean-errors (make-matrix O))
	  (tmp-O (make-matrix O))
	  (tmp-I (make-matrix I))
	  (delta-weights (make-matrix I))
	  (slopes (make-matrix I))
	  (prev-slopes (make-matrix I))
	  (signs (make-matrix O))
	  (sse 0.0)
	  (act-fn-deriv (deriv-fn-name act-fn))
	  (candidates nil))

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
      (dotimes (i (param params :cc-candidates))
	(let ((corr (- most-negative-fixnum (param params :cc-thr) 1))
	      (prev-corr most-negative-fixnum))
	  (m- delta-weights delta-weights)
	  (m- slopes slopes)
	  (m- prev-slopes prev-slopes)
	  (info "Training candidate:~%")
	  (map-matrix node-weights #'(lambda (x) (plain-rng -0.25 0.25)))
	  (dotimes (e (param params :cc-iter))
	    (let* ((eps (param params :cc-eps))
		   (mu (param params :cc-mu))
		   (shrink-factor (/ mu (1+ mu))))
	      ;; calculate node-vals for each pattern
	      (dotimes (j N)
		(let ((p (get-pattern full-patterns j)))
		  (setf (aref node-vals j) (funcall act-fn (inner-prod (first p) node-weights)))))
	      
	      (setf prev-corr corr
		    corr (cascor-corr node-vals net-errors :signs-dest signs))
	      (info "corr: ~A~%" corr)
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
				      (quickprop-update d s ps eps mu shrink-factor)))	  
	      (m- node-weights delta-weights)
	      (copy slopes prev-slopes)))
	  (push (cons corr (copy node-weights)) candidates)))
      (let ((best (first (sort candidates #'> :key #'car))))
	(info "corr: ~A~%" (car best))
	(cdr best)))))

(defun cascor-train-hidden (cascor patterns params)
  ;; check for unconnected node to be trained
  (assert (not (cascor-without-unconnected-node-p cascor)) nil "No unconnected nodes")
  (let ((full-patterns (cascor-full-patterns cascor patterns))
	(node (1- (cascor-hidden-num cascor))))
    (setf (cascor-node-weights cascor node)
	  (cascor-train-hidden-quickprop (cascor-output-weights cascor) full-patterns
					 (cascor-act-fn cascor) (copy-tree params)))))

(defun cascor-train (cascor train-patterns test-patterns nodes params)
  (cascor-train-output cascor train-patterns test-patterns params)
  (dotimes (n nodes)
    (info "Training ~A node~%"
	  (cascor-insert-node cascor))
    (cascor-train-hidden cascor train-patterns params)
    (info "Connecting to outputs~%")
    (cascor-connect-node cascor)
    (cascor-train-output cascor train-patterns test-patterns params))
  cascor)
