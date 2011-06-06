(in-package :annil)

(export '(classify simple-classifier simple-preproc-classifier classifier-range classifier-net
	  classifier-codec make-cascade-classifier improve-cascade-classifier))

;; single-network classifier

(defclass simple-classifier ()
  ((range :initarg :range :accessor classifier-range)
   (net :initarg :net :accessor classifier-net)))

(defmethod eval-network ((network simple-classifier) input)
  (eval-network (classifier-net network) input))

(defun %classify-patterns (classifier patterns)
  (let (correct incorrect)
    (do-patterns (patterns p)
      (let ((out (eval-network classifier (first p))))
	(if (< (abs (ammax (m- (copy out) (second p))))
	       (classifier-range classifier))
	    (push (list out (second p)) correct)
	    (push (list out (second p)) incorrect))))
    (list correct incorrect)))

(defun classify (classifier patterns)
  (cond ((single-input-p patterns) (eval-network classifier patterns))
	((single-pattern-p patterns)
	 (let ((out (eval-network classifier (first patterns))))
	   (values out
		   (< (abs (ammax (m- (copy out) (second patterns))))
		      (classifier-range classifier)))))
	(t (%classify-patterns classifier patterns))))

;; preproc classifier

(defclass simple-preproc-classifier (simple-classifier)
  ((codec :initarg :codec :accessor classifier-codec)))

(defmethod eval-network ((classifier simple-preproc-classifier) input)
  (eval-network (classifier-net classifier)
		(encode (classifier-codec classifier) (copy input))))

;; classifier creating

(defun make-cascade-classifier (patterns test-part act-fn classify-range hidden-num params cc-params)
  (make-instance 'simple-classifier
		 :net (cascade-train (make-cascade-network (patterns-input-dim patterns)
							   (patterns-output-dim patterns) act-fn nil)
				     patterns test-part hidden-num params cc-params)
		 :range classify-range))

(defun improve-cascor-classifier (classifier patterns test-part added-layers params cc-params)
  (cascade-train (classifier-net classifier) patterns test-part added-layers params cc-params)
  classifier)

