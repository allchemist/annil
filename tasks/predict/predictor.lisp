(in-package :annil)

(export '(ff-predictor make-ff-predictor make-cascade-ff-predictor make-adaboost-cascade-ff-predictor))

(defclass ff-predictor ()
  ((net :initarg :net :accessor pred-network)
   (preproc :initarg :preproc :accessor pred-preproc)
   (window  :initarg :window :accessor pred-window)))

(defmethod eval-network ((network ff-predictor) input)
  (eval-network (pred-network network) (encode (pred-preproc network) input)))

(defun make-ff-predictor (tseries window net-create-fn test-part &optional preproc-create-fn)
  (let* ((pred (make-instance 'ff-predictor :window window))
	 (pats (convert-patterns-to-svector (ts-unfold-to-patterns tseries window)))
	 (moments (map 'vector #'(lambda (x) (aref (first x) 0)) (ts-patterns tseries)))
	 (preproc (setf (pred-preproc pred) (when preproc-create-fn
					   (let ((codec (funcall preproc-create-fn pats)))
					     (setf pats (encode codec pats)) codec))))
	 (test-div (when test-part (floor (* (- 1 test-part) (length pats)))))
	 (train (if test-part (subseq pats 0 test-div) pats))
	 (test (when test-part (subseq pats (1+ test-div)))))
    (setf (pred-network pred) (funcall net-create-fn (encode preproc train)))
    (when test-part
      (let* ((coords (last (coerce moments 'list) (length test)))
	     (new (map 'list #'(lambda (m p) (list m (aref (eval-network pred (first p)) 0)))
		       coords test)))
	(gplt:make-plot (list ;(map 'list #'(lambda (p) (list (aref (first p) 0)
			;				     (aref (second p) 0)))
			;	   train)
			      (map 'list #'(lambda (m p) (list m (aref (second p) 0)))
				   coords test)
			      new)
			:multi '('red 'blue) :with :lines
			:ranges `((,(first coords) ,(car (last coords)))
				  ,(let ((outs (map 'list #'(lambda (x) (aref (second x) 0)) pats)))
				     `(,(* (apply #'min outs) 1.3) ,(* (apply #'max outs) 1.3)))))))
    pred))

(defun make-cascade-ff-predictor (tseries window test-part)
  (make-ff-predictor
   tseries window
   #'(lambda (p)
       (make-cascade-classifier
	p 0.1
	'((:act-fn . :tanh) (:eps . 0.2) (:mu . 2.0) (:epochs . 200) (:thr . 1.e-5) (:recompute . 10)
	  (:verbosity . 2) (:max-err-bits . 0.05) (:misclassify-limit . 0.1))
	'((:eps . 1.0) (:mu . 2.0) (:epochs . 200) (:thr . 1.e-5) (:recompute . 10) (:verbosity . 0)
	  (:candidates . 10) (:hidden-num . 20) (:epochs-handicap . 10))))
   test-part))

(defun make-adaboost-cascade-ff-predictor (tseries window test-part)
  (make-ff-predictor
   tseries window
   #'(lambda (p)
       (make-adaboost-network p
	 #'(lambda (x)
	     (make-cascade-classifier p 0.1
	       '((:act-fn . :tanh) (:eps . 0.2) (:mu . 2.0) (:epochs . 200) (:thr . 1.e-5) (:recompute . 10)
		 (:verbosity . 2) (:max-err-bits . 0.1) (:misclassify-limit . 0.05))
	       '((:eps . 1.0) (:mu . 2.0) (:epochs . 200) (:thr . 1.e-5) (:recompute . 10) (:verbosity . 0)
		 (:candidates . 10) (:hidden-num . 20) (:epochs-handicap . 10))))
	 '((:verbosity . t) (:max-weak . 3) (:misclassify-limit . 0.05))))
   test-part))

(defun visual-test-ts (network ts window)
  (let ((pats (ts-unfold-to-patterns ts window)))
    (gplt:make-plot
     (map 'list #'(lambda (x) (list x (aref (eval-network network (first x)) 0))) pats)
     :with :lines)))
