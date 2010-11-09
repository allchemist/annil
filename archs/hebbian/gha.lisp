(in-package :annil)

(defun make-gha-weights (input-dim output-dim)
  ;; init random gha weights
  (map-matrix (make-random-matrix (list output-dim input-dim))
	      #'(lambda (x) (float (/ (1- x) 4)))))

(defun gha-eval (weights input)
  (gemv weights input))

(defun gha-eval-patterns (weights patterns)
  (etypecase patterns
    (matrix-inputs (transpose (gemm weights patterns :transb :trans)))
    (matrix-patterns (list (transpose (gemm weights (first patterns) :transb :trans)) (second patterns)))
    (list-inputs (mapcar #'(lambda (i) (gemv weights i)) patterns))
    (list-patterns (mapcar #'(lambda (i) (list (gemv weights (first i)) (second i))) patterns))))

(defun gha-takeback (weights output)
  (gemv weights output :transa :trans))

(defun gha-takeback-patterns (weights patterns)
  (etypecase patterns
    (matrix-inputs (gemm patterns weights))
    (matrix-patterns (list (gemm (first patterns) weights) (second patterns)))
    (list-inputs (mapcar #'(lambda (o) (gemv weights o :transa :trans)) patterns))
    (list-patterns (mapcar #'(lambda (o) (list (gemv weights (first o) :transa :trans) (second o))) patterns))))

(defun gha-adjust (weights input rate)
  (let ((output (gha-eval weights input))
	oo-space oi-space w-space)
    (setf oo-space (make-matrix `(,(length output) ,(length output)))
	  oi-space (make-matrix `(,(length output) ,(length input)))
	  w-space (make-matrix-like weights))
    (m+ weights
	(m*c (m- (ger output input :dest oi-space)
		 (trmm (ger output output :dest oo-space)
		       (copy weights w-space) :uplo :lower))
	     rate))))

(defun gha-epoch (weights patterns rate)
  (do-patterns-shuffle (patterns p)
    (gha-adjust weights (if (listp p) (first p) p) rate))
  weights)

(defun gha-error (weights patterns)
  (let ((err 0))
    (do-patterns (patterns p)
      (let ((i (if (listp p) (first p) p)))
	(incf err (msum (map-matrix-square (m- (gemv weights (gemv weights i) :transa :trans) i))))))
    err))

(defun gha (weights patterns params)
  (let ((prev-weights (copy weights))
	(prev-err most-positive-fixnum)
	(eps (param params :eps)))
    (dotimes (e (param params :iter))
;      (info "Epoch: ~A~%" e)
      (gha-epoch weights patterns (param params :rate))
;      (m*c weights 0.99)
      (let ((err (gha-error weights patterns)))
	(info "Error: ~A~%" err)
	(if (< (abs (- err prev-err)) eps)
	    (return)
	    (setf prev-err err)))))
  weights)
