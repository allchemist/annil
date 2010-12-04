(in-package :annil)  

(export '(classifier classify-patterns
	  cascor-classifier make-cascor-classifier improve-cascor-classifier))

;; general classifier

(defclass classifier (tool)
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

#|
;; mlp classifier

(defclass mlp-classifier (classifier)
  ((net :initarg :net :accessor classifier-net)))

(defun make-mlp-classifier (patterns act-fn classifier-range hidden-layers epochs params)
  (make-instance 'mlp-classifier
		 :net (mlp-train (make-random-mlp `(,(patterns-input-dim patterns)
						    ,@hidden-layers
						    ,(patterns-output-dim patterns))
						  act-fn)
				 patterns epochs params)
		 :range classifier-range))
|#

;; slp classifier

(defclass slp-classifier (classifier)
  ((net :initarg :net :accessor classifier-net)))

(defun make-slp-classifier (train-patterns test-patterns act-fn params classifier-range)
  (make-instance 'slp-classifier
		 :net (slp-train (make-random-slp (patterns-input-dim train-patterns)
						  (patterns-output-dim train-patterns)
						  act-fn)
				 train-patterns test-patterns params 'quickprop)
		 :range classifier-range))



;; cascor classifier

(defclass cascor-classifier (classifier)
  ((net :initarg :net :accessor classifier-net)))

(defun make-cascor-classifier (train-patterns test-patterns act-fn classifier-range hidden-layers params &optional test-fn)
  (make-instance 'cascor-classifier
		 :net (cascor-train (make-random-cascor (patterns-input-dim train-patterns)
							(patterns-output-dim train-patterns) act-fn)
				    train-patterns test-patterns hidden-layers params test-fn)
		 :range classifier-range))

(defun improve-cascor-classifier (classifier train-patterns test-patterns added-layers params &optional test-fn)
  (cascor-train (classifier-net classifier) train-patterns test-patterns added-layers params test-fn)
  classifier)
