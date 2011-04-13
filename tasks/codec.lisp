(in-package :annil)

(export '(encode decode chain-codec codecs-in-chain make-chain-codec
	  autoscale-codec make-autoscale-codec autoscale-means autoscale-norms
	  pca-codec pca-loadings pca-scores make-pca-codec reduce-pca-codec
	  store-pca-codec restore-pca-codec))

(defgeneric encode (codec patterns))
(defgeneric decode (codec patterns))

;; chain codec

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
		       (map 'simple-vector #'(lambda (i) (gemv -P i :transa :trans)) patterns)
		       (map 'simple-vector #'(lambda (p) (list (gemv -P (first p) :transa :trans) (copy (second p)))) patterns))))

(defmethod decode ((codec pca-codec) patterns)
  (let ((-P (pca-loadings codec)))
    (patterns-dispatch (patterns)
		       (gemv -P patterns)
		       (list (gemv -P patterns) (copy (second patterns)))
		       (gemm patterns -P :transb :trans)
		       (list (gemm (first patterns) -P :transb :trans) (copy (second patterns)))
		       (map 'simple-vector #'(lambda (i) (gemv -P i)) patterns)
		       (map 'simple-vector #'(lambda (p) (list (gemv -P (first p)) (copy (second p)))) patterns))))

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

(defun store-pca-codec (codec path)
  (with-open-file (s path
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
    (princ "annil pca codec storage" s)
    (store-matrix (pca-scores codec) s t)
    (store-matrix (pca-loadings codec) s t)))

(defun restore-pca-codec (path)
  (with-open-file (s path)
    (assert (string= (read-line s) "annil pca codec storage") nil "Not a pca codec storage")
    (make-instance 'pca-codec
		   :scores (restore-matrix s)
		   :loadings (restore-matrix s))))
