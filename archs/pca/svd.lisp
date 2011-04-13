(in-package :annil)

(export 'pca-svd)

(defun pca-svd (matrix components &key verbosity)
  "Return scores (T), then loadings (P)"
  (when (not (typep verbosity 'fixnum)) (setf verbosity 0))
  (when components
    (assert (<= components (dim1 matrix)) nil "Trying to extract too much principal components"))
  (when (>= verbosity 1)
    (info "Performing SVD extraction of ~A principal components.~%" components))
  (multiple-value-bind (S U VT)
      (mth::svd (copy matrix) :left :all :right :all :values :matrix)
    (values
      (let ((tmp (gemm U S)))
	(if components
	    (adjust-array tmp (list (dim0 tmp) components))
	    tmp))
      (let ((tmp (transpose VT)))
	(if components
	    (adjust-array tmp (list (dim0 tmp) components))
	    tmp)))))
