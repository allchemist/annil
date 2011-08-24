(in-package :annil)

(defclass pca-codec ()
  ((loadings :initarg :loadings :accessor pca-loadings)
   (scores   :initarg :scores   :accessor pca-scores)))

(defmethod encode ((codec pca-codec) patterns)
  (let ((-P (pca-loadings codec)))
    (patterns-dispatch (patterns)
		       (gemv -P patterns :transa :trans)
		       (cons (gemv -P patterns :transa :trans) (cdr patterns))
		       (gemm patterns -P)
		       (cons (gemm (first patterns) -P) (cdr patterns))
		       (map 'simple-vector #'(lambda (i) (gemv -P i :transa :trans)) patterns)
		       (map 'simple-vector #'(lambda (p) (cons (gemv -P (first p) :transa :trans) (cdr p)))
			    patterns))))

(defmethod decode ((codec pca-codec) patterns)
  (let ((-P (pca-loadings codec)))
    (patterns-dispatch (patterns)
		       (gemv -P patterns)
		       (cons (gemv -P patterns) (cdr patterns))
		       (gemm patterns -P :transb :trans)
		       (cons (gemm (first patterns) -P :transb :trans) (cdr patterns))
		       (map 'simple-vector #'(lambda (i) (gemv -P i)) patterns)
		       (map 'simple-vector #'(lambda (p) (cons (gemv -P (first p)) (cdr p)))
			    patterns))))

(defun make-svd-pca-codec (patterns &optional components verbosity)
  (assert (and (not (single-input-p patterns))
	       (not (single-pattern-p patterns)))
	  nil "PCA for single input makes no sense")
  (let ((data (convert-patterns-to-matrix patterns)))
    (multiple-value-bind (-T -P)
	(pca-svd (if (listp data) (first data) data) components :verbosity verbosity)
      (make-instance 'pca-codec :loadings -P :scores -T))))

(defun make-nipals-pca-codec (patterns &optional components verbosity)
  (assert (and (not (single-input-p patterns))
	       (not (single-pattern-p patterns)))
	  nil "PCA for single input makes no sense")
  (let ((data (convert-patterns-to-matrix patterns)))
    (multiple-value-bind (-T -P)
	(nipals (copy (if (listp data) (first data) data)) components :verbosity verbosity)
      (make-instance 'pca-codec
		     :loadings -P :scores -T))))

;; pca codec creating

;; params: method verbosity components
(defun make-pca-codec (patterns params)
  (funcall
   (case (param params :method)
     (:svd #'make-svd-pca-codec)
     (:nipals #'make-nipals-pca-codec))
   patterns (param params :components) (param params :verbosity)))

(defun reduce-pca-codec (codec new-dim)
  (let ((orig-loadings (pca-loadings codec))
	(orig-scores (pca-scores codec)))
    (make-instance 'pca-codec
		   :scores (adjust-array (copy orig-scores)
					 (list (dim0 orig-scores) new-dim))
		   :loadings (adjust-array (copy orig-loadings)
					   (list (dim0 orig-loadings) new-dim)))))
