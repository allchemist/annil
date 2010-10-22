(in-package :annil)

(defgeneric encode (codec patterns))

;; autoscale

(defclass autoscale-codec ()
  ((means :initarg :means :accessor autoscale-means)
   (norms :initarg :norms :accessor autoscale-norms)))

(defun make-autoscale-codec (patterns)
  (let ((params (autoscale-params patterns)))
    (make-instance 'autoscale-codec :means (first params) :norms (second params))))

(defmethod encode ((codec autoscale-codec) patterns)
  (autoscale-patterns patterns (autoscale-means codec) (autoscale-norms codec)))

(defmethod decode ((codec autoscale-codec) patterns)
  (undo-autoscale-patterns patterns (autoscale-means codec) (autoscale-norms codec)))

;; gha

(defclass gha-codec ()
  ((weights :initarg :weights :accessor gha-weights)))

(defun make-gha-codec (inputs out-dim rate epochs eps)
  (make-instance 'gha-codec :weights
		 (gha (make-gha-weights inputs out-dim)
		      inputs rate epochs eps)))

(defun improve-gha-codec (gha-codec inputs rate epochs eps)
  (gha (gha-weights gha-codec) inputs rate epochs eps))

(defmethod encode ((codec gha-codec) (patterns vector))
  (gha-eval (gha-weights codec) patterns))

(defmethod encode ((codec gha-codec) (patterns array))
  (gha-eval-patterns (gha-weights codec) patterns))

(defmethod encode ((codec gha-codec) (patterns list))
  (mapcar #'(lambda (i) (gha-eval (gha-weights codec) i)) patterns))

(defmethod decode ((codec gha-codec) (patterns vector))
  (gha-takeback (gha-weights codec) patterns))

(defmethod decode ((codec gha-codec) (patterns array))
  (gha-takeback-patterns (gha-weights codec) patterns))

(defmethod decode ((codec gha-codec) (patterns list))
  (mapcar #'(lambda (o) (gha-takeback (gha-weights codec) o)) patterns))
