(in-package :annil)

(export '(encode decode chain-codec make-chain-codec
	  autoscale-codec make-autoscale-codec
	  centering-codec make-centering-codec
	  bound-scale-codec make-bound-scale-codec

	  pca-codec make-pca-codec reduce-pca-codec))

(defgeneric encode (codec patterns))
(defgeneric decode (codec patterns))

;; null codec

(defmethod encode ((codec (eql nil)) patterns) patterns)
(defmethod decode ((codec (eql nil)) patterns) patterns)

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

;; centering

(defclass centering-codec ()
  ((means :initarg :means :accessor centering-means)))

(defmethod encode ((codec centering-codec) patterns)
  (centering patterns (centering-means codec)))

(defmethod decode ((codec centering-codec) patterns)
  (undo-centering patterns (centering-means codec)))

(defun make-centering-codec (patterns)
  (make-instance 'centering-codec :means (centering-params patterns)))

;; output

(defclass bound-scale-codec ()
  ((bounds :initarg :bounds :accessor scale-bounds)))

(defmethod encode ((codec bound-scale-codec) patterns)
  (bound-scale-patterns-outputs patterns (scale-bounds codec)))

(defmethod decode ((codec bound-scale-codec) patterns)
  (undo-bound-scale-patterns-outputs patterns (scale-bounds codec)))

(defun make-bound-scale-codec (patterns interval &optional old-interval)
  (make-instance 'bound-scale-codec :bounds (bound-scale-outputs-params patterns interval old-interval)))

(defmethod encode ((codec (eql :log-scale-codec)) patterns)
  (log-scale-patterns-outputs patterns))

(defmethod decode ((codec (eql :log-scale-codec)) patterns)
  (log-unscale-patterns-outputs patterns))
