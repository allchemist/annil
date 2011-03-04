(in-package :annil)

(defun pca-svd (matrix &optional components)
  "Return scores (T), then loadings (P)"
  (when components
    (assert (<= components (dim1 matrix)) nil "Trying to extract too much principal components"))
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
