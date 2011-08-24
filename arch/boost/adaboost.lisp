(in-package :annil)

(export '(adaboost-network make-adaboost-network))

(defclass adaboost-network ()
  ((classifiers :initarg :classifiers :accessor adaboost-classifiers)))

(defmethod eval-network ((network adaboost-network) input)
  (let ((sum 0.0)
	(denom 0.0))
    (dolist (c (adaboost-classifiers network))
      (incf sum (* (second c)
		   (aref (eval-network (first c) input) 0)))
      (incf denom (abs (second c))))
    (make-matrix 1 :initial-element (/ sum denom))))

(defun adaboost-network-quality (network pats misclassify-limit)
  (let ((err 0.0))
    (do-patterns-safe (pats p)
      (let ((goal (aref (second p) 0))
	    (out (aref (eval-network network (first p)) 0)))
	(when (> (abs (- out goal)) misclassify-limit)
	  (incf err (tanh (square (* (- goal out) (pattern-weight p))))))))
    (sqrt err)))

(defun make-adaboost-network (patterns weak-train-fn params)
  (assert (= (patterns-output-dim patterns) 1) nil "Now only single-output adaboost is implemented")
  (let (cls
	(npats (num-patterns patterns))
	(misclassify-limit (or (param params :misclassify-limit) 1.0))
	(pats (convert-patterns-to-svector patterns))
	(verbosity (param params :verbosity))
	(neg/pos 1.0))
    (when verbosity (info "Initiating AdaBoost classifier training~%"))
    (if (not (patterns-with-weights-p patterns))
	(progn (dotimes (i npats)
		 (nconc (svref pats i) `(,(make-matrix 1 :initial-element (float (/ npats))))))
	       (normalize-patterns-weights pats))
	(let ((w (coerce (sort (remove-duplicates (map 'vector #'pattern-weight pats)) #'>) 'list)))
	  (setf neg/pos (/ (car w) (car (last w))))))
    (dotimes (n (param params :max-weak))
      (when verbosity (info "Training ~A'th weak classifier~%" (1+ n)))
      (let* ((c (funcall weak-train-fn pats))
	     (q (adaboost-network-quality c pats misclassify-limit))
	     (a (* 0.5 (log (/ (- 1 q) (+ q 1.e-7))))))
	(when (zerop q)
	  (info "Got perfect classifier from weak training, stop")
	  (return-from make-adaboost-network 'error))
	(do-patterns-safe/sv (pats p i)
	  (setf (pattern-weight p)
		(* (pattern-weight p)
		   (exp (- (* a
			      (aref (second p) 0)
			      (aref (eval-network c (first p)) 0)))))))
	(normalize-patterns-weights pats)
	(push (list c a) cls)))
    (let ((abc (make-instance 'adaboost-network :classifiers (nreverse cls))))
      (when verbosity
	(multiple-value-bind (err neg-bits pos-bits)
	    (compute-network-err abc patterns 1.0)
	  (info "Boosting quality: error-sum= ~A, error-bits= ~A / ~A~%" err neg-bits pos-bits)))
      abc)))