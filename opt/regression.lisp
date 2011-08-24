(in-package :annil)

(defun cv-split-patterns (patterns test-part &optional (sequent nil))
  (let (train test)
    (if sequent
	(let* ((npats (num-patterns patterns))
	       (div (floor (* npats (- 1.0 test-part)))))
	  (dotimes (i npats)
	    (let ((p (get-pattern patterns i)))
	      (if (<= i div)
		  (push p train)
		  (push p test)))))
	(do-patterns-shuffle (patterns p)
	  (if (< (random 1.0) test-part)
	      (push p test)
	      (push p train))))
    (values (coerce train 'simple-vector)
	    (coerce test  'simple-vector))))

(defun optimal-test-part-size (num-params &optional num-patterns)
  (if (and num-patterns (> num-patterns (* num-params 30)))
      0.0
      (/ (- (sqrt (- (* 2 num-params) 1)) 1)
	 (* 2 (- num-params 1)))))

(defun update-params (params method)
  (let ((copy-params (copy-tree params)))
    (case method
      (:quickprop
	 (setf (param copy-params :eps) (/ (param copy-params :eps) 2)
	       (param copy-params :mu) (/ (param copy-params :mu) 2)
	       (param copy-params :thr) (/ (param copy-params :thr) 5)))
      (:rprop
	 (setf (param copy-params :d0) (/ (param copy-params :d0) 5)
	       (param copy-params :dmin) (/ (param copy-params :dmin) 5)
	       (param copy-params :dmax) (/ (param copy-params :dmax) 2)
	       (param copy-params :thr) (/ (param copy-params :thr) 5))))
    copy-params))

(defun optimize-with-restarts (w method slope-fn err-fn train-patterns test-patterns params)
  (let ((restarts (or (param params :restarts) 0))
	(err most-positive-fixnum)
	(epochs 0)
	(copy-params (copy-tree params))
	(optimize-fn (intern (string method) :annil)))
    (if (zerop restarts)
	(funcall optimize-fn w slope-fn err-fn train-patterns test-patterns copy-params)
	(progn
	  (decf (param copy-params :verbosity))
	  (dotimes (i (1+ restarts))
	    (multiple-value-bind (cur-w cur-err cur-epochs)
		(funcall optimize-fn w slope-fn err-fn train-patterns test-patterns copy-params)
	      (declare (ignore cur-w))
	      (update-params params method)
	      (incf epochs cur-epochs)
	      (setf err cur-err
		    epochs cur-epochs)))
	  (incf (param copy-params :verbosity))
	  (values w err epochs)))))
