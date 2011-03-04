(in-package :annil)

(defgeneric encode (codec patterns))
(defgeneric decode (codec patterns))

;; autoscale

(defclass autoscale-codec ()
  ((means :initarg :means :accessor autoscale-means)
   (norms :initarg :norms :accessor autoscale-norms)))

(defmethod encode ((codec autoscale-codec) patterns)
  (autoscale patterns (autoscale-means codec) (autoscale-norms codec)))

(defmethod decode ((codec autoscale-codec) patterns)
  (undo-autoscale patterns (autoscale-means codec) (autoscale-norms codec)))

(defun make-autoscale-codec (patterns)
  (let ((params (autoscale-params patterns)))
    (make-instance 'autoscale-codec :means (first params) :norms (second params))))


;; pca

(defclass pca-codec ()
  ((loadings :initarg :loadings :accessor pca-loadings)
   (scores   :initarg :scores   :accessor pca-scores)))

(defmethod encode ((codec pca-codec) patterns)
  (let ((-P (pca-loadings codec)))
    (patterns-dispatch (patterns)
		       (gemv -P patterns :transa :trans)
		       (list (gemv -P patterns :transa :trans) (copy (second patterns)))
		       (gemm patterns -P)
		       (list (gemm (first patterns) -P) (copy (second patterns)))
		       (mapcar #'(lambda (i) (gemv -P i :transa :trans)) patterns)
		       (mapcar #'(lambda (p) (list (gemv -P (first p) :transa :trans) (copy (second p)))) patterns))))

(defmethod decode ((codec pca-codec) patterns)
  (let ((-P (pca-loadings codec)))
    (patterns-dispatch (patterns)
		       (gemv -P patterns)
		       (list (gemv -P patterns) (copy (second patterns)))
		       (gemm patterns -P :transb :trans)
		       (list (gemm (first patterns) -P :transb :trans) (copy (second patterns)))
		       (mapcar #'(lambda (i) (gemv -P i)) patterns)
		       (mapcar #'(lambda (p) (list (gemv -P (first p)) (copy (second p)))) patterns))))

(defun make-svd-pca-codec (patterns &optional components)
  (assert (and (not (single-input-p patterns))
	       (not (single-pattern-p patterns)))
	  nil "PCA for single input makes no sense")
  (let ((data (convert-patterns-to-matrix patterns)))
    (multiple-value-bind (-T -P)
	(pca-svd (if (listp data) (first data) data) components)
      (make-instance 'pca-codec :loadings -P :scores -T))))

(defun make-nipals-pca-codec (patterns &optional components)
  (assert (and (not (single-input-p patterns))
	       (not (single-pattern-p patterns)))
	  nil "PCA for single input makes no sense")
  (let ((data (convert-patterns-to-matrix patterns)))
    (multiple-value-bind (-T -P)
	(nipals (copy (if (listp data) (first data) data)) components)
      (make-instance 'pca-codec
		     :loadings (convert-patterns-to-matrix -P)
		     :scores   (convert-patterns-to-matrix -T)))))

;; codec chain

(defclass chain-codec ()
  ((codecs :initarg :codecs :accessor codecs-in-chain)))

(defun make-chain-codec (&rest codecs)
  (make-instance 'chain-codec :codecs codecs))

(defmethod encode ((codec chain-codec) patterns)
  (labels ((apply-codecs-chain (chain pats)
	     (if (null chain) pats
		 (apply-codecs-chain
		  (cdr chain)
		  (encode (car chain) pats)))))
    (apply-codecs-chain (codecs-in-chain codec) patterns)))

(defmethod decode ((codec chain-codec) patterns)
  (labels ((apply-codecs-chain (chain pats)
	     (if (null chain) pats
		 (apply-codecs-chain
		  (cdr chain)
		  (decode (car chain) pats)))))
    (apply-codecs-chain (reverse (codecs-in-chain codec)) patterns)))
