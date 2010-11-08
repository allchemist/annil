(in-package :annil)

(defgeneric encode (codec patterns))

;; autoscale

(defclass autoscale-codec ()
  ((means :initarg :means :accessor autoscale-means)
   (norms :initarg :norms :accessor autoscale-norms)))

(defun make-autoscale-codec (patterns)
  (let* ((inputs (convert-patterns-to-matrix patterns))
	 (params (autoscale-params (if (listp inputs) (first inputs) inputs))))
    (make-instance 'autoscale-codec :means (first params) :norms (second params))))

(defmethod encode ((codec autoscale-codec) patterns)
  (if (vectorp patterns)
      (autoscale-input patterns (autoscale-means codec) (autoscale-norms codec))
      (if (vectorp (first patterns))
	  (list (autoscale-input (first patterns) (autoscale-means codec) (autoscale-norms codec))
		(second patterns))
	  (autoscale-patterns patterns (autoscale-means codec) (autoscale-norms codec)))))

(defmethod decode ((codec autoscale-codec) patterns)
  (if (vectorp patterns)
      (undo-autoscale-input patterns (autoscale-means codec) (autoscale-norms codec))
      (if (vectorp (first patterns))
	  (list (undo-autoscale-input (first patterns) (autoscale-means codec) (autoscale-norms codec))
		(second patterns))
	  (undo-autoscale-patterns patterns (autoscale-means codec) (autoscale-norms codec)))))

;; gha

(defclass gha-codec ()
  ((weights :initarg :weights :accessor gha-weights)))

(defun make-gha-codec (patterns out-dim params)
  (make-instance 'gha-codec :weights
		 (gha (make-gha-weights (patterns-input-dim patterns) out-dim)
		      patterns params)))

(defun improve-gha-codec (gha-codec patterns params)
  (gha (gha-weights gha-codec) patterns params)
  gha-codec)

(defmethod encode ((codec gha-codec) patterns)
  (if (vectorp patterns)
      (gha-eval (gha-weights codec) patterns)
      (if (vectorp (first patterns))
	  (list (gha-eval (gha-weights codec) (first patterns)) (second patterns))
	  (gha-takeback-patterns (gha-weights codec) patterns))))

(defmethod decode ((codec gha-codec) patterns)
  (if (vectorp patterns)
      (gha-takeback (gha-weights codec) patterns)
      (if (vectorp (first patterns))
	  (list (gha-takeback (gha-weights codec) (first patterns)) (second patterns))
	  (gha-takeback-patterns (gha-weights codec) patterns))))
