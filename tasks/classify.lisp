(in-package :annil)

(defclass classifier ()
  ((range :initarg :range :accessor classifier-range)))

(defun classify-patterns (classifier patterns &optional verbosity)
  (let ((correct nil)
	(incorrect nil)
	(thr (classifier-range classifier)))
    (do-patterns (patterns p)
      (let ((out (eval-network (classifier-net classifier) (first p))))
	(if (< (abs (ammax (m- (copy out) (second p)))) thr)
	    (push (list (copy out) (second p)) correct)
	    (push (list (copy out) (second p)) incorrect))))
    (case verbosity
      (:none nil)
      (:stat (format *query-io* "Correctly classified: ~A patterns,~%Incorrectly classified or undecided: ~A patterns~%" (length correct) (length incorrect)))
      (:full
	 (progn
	   (format *query-io* "Correct:~%")
	   (dolist (l correct)
	     (print-matrix (first l) :dest *query-io*))
	   (format *query-io* "~%Incorrect:~%")
	   (dolist (l incorrect)
	     (print-matrix (first l) :dest *query-io*))
	   (format *query-io* "~%Correctly classified: ~A patterns,~%Incorrectly classified or undecided: ~A patterns~%" (length correct) (length incorrect)))))
    (list (length correct) (length incorrect))))

;; cascor classifier

(defclass cascor-classifier (classifier)
  ((net :initarg :net :accessor classifier-net)))

(defmethod eval-network ((network cascor-classifier) input)
  (eval-network (classifier-net network) input))

(defun make-cascor-classifier (train-patterns test-patterns act-fn classify-range hidden-layers params cc-params)
  (make-instance 'cascor-classifier
		 :net (cascor-train (make-random-cascor (patterns-input-dim train-patterns)
							(patterns-output-dim train-patterns) act-fn)
				    train-patterns test-patterns hidden-layers params cc-params)
		 :range classify-range))

(defun improve-cascor-classifier (classifier train-patterns test-patterns added-layers params cc-params)
  (cascor-train (classifier-net classifier) train-patterns test-patterns added-layers params cc-params)
  classifier)
