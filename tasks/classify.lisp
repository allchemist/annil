(in-package :annil)

(export '(classify simple-classifier simple-preproc-classifier classifier-range classifier-net
	  classifier-codec boost-classifier boost-classifiers boost-output-ranges
	  make-cascor-classifier improve-cascor-classifier))

(defgeneric classify (classifier patterns))

;; single-network classifier

(defclass simple-classifier ()
  ((range :initarg :range :accessor classifier-range)
   (net :initarg :net :accessor classifier-net)))

(defmethod eval-network ((network simple-classifier) input)
  (eval-network (classifier-net network) input))

(defun %classify-simple (classifier patterns)
  (let (correct incorrect)
    (do-patterns (patterns p)
      (let ((out (eval-network classifier (first p))))
	(if (< (abs (ammax (m- (copy out) (second p))))
	       (classifier-range classifier))
	    (push (list out (second p)) correct)
	    (push (list out (second p)) incorrect))))
    (list correct incorrect)))

(defmethod classify ((classifier simple-classifier) patterns)
  (%classify-simple classifier patterns))

;; preproc classifier

(defclass simple-preproc-classifier (simple-classifier)
  ((codec :initarg :codec :accessor classifier-codec)))

(defmethod eval-network ((classifier simple-preproc-classifier) input)
  (eval-network (classifier-net classifier)
		(encode (classifier-codec classifier) (copy input))))

(defmethod classify ((classifier simple-preproc-classifier) patterns)
  (%classify-simple classifier (encode (classifier-codec classifier) (copy-patterns patterns))))

;; boost classifier

(defclass boost-classifier ()
  ((classifiers :initarg :classifiers :accessor boost-classifiers)
   (output-ranges :initarg :output-ranges :accessor boost-output-ranges)))

(defmethod eval-network ((network boost-classifier) input)
  (let ((ranges (boost-output-ranges network)))
    (m+c
     (reduce #'m*
	     (loop for c in (boost-classifiers network) collect
	       (m-c (eval-network c input) (first ranges))))
     (first ranges))))

(defmethod classify ((classifier boost-classifier) patterns)
  (%classify-simple classifier patterns))

;; KLUDGE!
(defmethod classifier-range ((classifier boost-classifier))
  (classifier-range (first (boost-classifiers classifier))))


;; classifier creating

(defun make-cascor-classifier (patterns test-part act-fn classify-range hidden-layers params cc-params)
  (make-instance 'simple-classifier
		 :net (cascor-train (make-random-cascor (patterns-input-dim patterns)
							(patterns-output-dim patterns) act-fn)
				    patterns test-part hidden-layers params cc-params)
		 :range classify-range))

(defun improve-cascor-classifier (classifier patterns test-part added-layers params cc-params)
  (cascor-train (classifier-net classifier) patterns test-part added-layers params cc-params)
  classifier)

