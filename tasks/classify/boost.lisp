(in-package :annil)

(export '(boost-classifier boost-classifiers boost-preproc))

(defclass boost-classifier ()
  ((classifiers :initarg :classifiers :accessor boost-classifiers)
   (preproc :initarg :preproc :accessor boost-preproc)))

(defmethod eval-network ((network boost-classifier) input)
  (let ((sum 0.0)
	(preproc (boost-preproc network)))
    (dolist (c (boost-classifiers network))
      (incf sum (* (second c)
		   (aref (eval-network (first c) (if preproc (encode preproc input) input)) 0))))
    (make-matrix 1 :initial-element (signum sum))))

(defun %standard-deviation (data mean)
  (let ((var 0.0))
    (dotimes (i (dim0 data))
      (incf var (square (- (aref data i) mean))))
    (sqrt (/ var (dim0 data)))))
