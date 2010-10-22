(in-package :annil)

(defun low-triangle (matrix)
  ;; set every element on or above diagonal to zero
  (let ((dim (dim0 matrix))
	(element-type (array-element-type matrix)))
    (assert (= dim (dim1 matrix)) nil "Matrix should be square")
    (let ((zero (coerce 0 element-type)))
      (dotimes (i dim)
	(dotimes (j i)
	  (setf (aref matrix i j) zero)))))
  matrix)

(defun make-gha-weights (inputs output-dim)
  ;; init random gha weights
  (make-random-matrix (list output-dim (dim1 inputs))
		      :rng #'(lambda (x) (declare (ignore x)) (plain-rng -0.25 0.25))))

(defun gha-eval (weights input)
  (gemv weights input))

(defun gha-eval-patterns (weights patterns)
  (transpose (gemm weights patterns :transb :trans)))

(defun gha-takeback (weights output)
  (gemv weights output :transa :trans))

(defun gha-takeback-patterns (weights outputs)
  (gemm outputs weights))

(defun gha-adjust (weights input rate)
  (let ((output (gha-eval weights input)))
    (m+ weights
	(m*c (m- (ger output input)
		 (gemm (low-triangle (ger output output)) weights))
	     rate))))

(defun gha-epoch (weights patterns rate)
  (do-patterns-shuffle (patterns p)
    (gha-adjust weights p rate))
  weights)

(defun gha (weights patterns rate epochs eps)
  (let ((prev-weights (copy weights)))
    (dotimes (e epochs)
      (gha-epoch weights patterns rate)
      (if (~= weights prev-weights eps)
	  (return weights)
	  (setf rate (/ rate 1.01)))))
  weights)
