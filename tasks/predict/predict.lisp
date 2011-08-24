(in-package :annil)

(export '(make-predictor make-cascade-predictor make-adaboost-cascade-predictor make-rcascade-predictor
	  make-prediction-series))

(defclass predictor ()
  ((net :initarg :net :accessor pred-net)
   (window :initarg :window :accessor pred-window)
   (latest-pat :initarg :latest-pat :accessor pred-latest-pat)
   (tmp-pat :initarg :tmp-pat :accessor pred-tmp-pat)
   (scale-bounds :initarg :scale-bounds :accessor pred-scale-bounds)
   (log-p :initarg :log-p :accessor pred-log-p)
   (ops :initarg :ops :accessor pred-ops)))

(defun make-predictor (tseries window net-create-fn test-part &optional log)
  (let ((pure-pats (make-tseries-patterns tseries window))
	(mod-tseries tseries)
	scale-bounds pats)
    (when log (setf mod-tseries (log-tseries mod-tseries)))
    (multiple-value-setq
	(mod-tseries scale-bounds)
      (scale-tseries (diff-tseries mod-tseries)))
    (setf pats (make-tseries-patterns mod-tseries window))
    (let* ((test-div (when test-part (floor (* (- 1 test-part) (length pats)))))
	   (train (if test-part (subseq pats 0 test-div) pats))
	   (test (when test-part (subseq pats (1+ test-div))))
	   (pred (funcall net-create-fn train)))
      (when test-part
	(let* ((coords (iota (length test)))
	       (aim (map 'list #'(lambda (m p) (list m (e1 (second p)))) coords test))
	       (new (map 'list #'(lambda (m p) (list m (e1 (eval-network pred (first p))))) coords test)))
	  (gplt:make-plot (list aim new)
			  :multi '('red 'blue) :with :lines
			  :ranges `((0 ,(aref coords (1- (length coords))))
				    ,(let ((outs (map 'sf-seq #'(lambda (x) (e1 (second x))) pats)))
				       `(,(* (mmin outs) 1.3) ,(* (mmax outs) 1.3)))))))
      (make-instance 'predictor
		     :net pred
		     :latest-pat (svref pure-pats (1- (length pure-pats)))
		     :tmp-pat nil
		     :ops nil
		     :window window
		     :log-p log
		     :scale-bounds scale-bounds))))

(defun make-cascade-predictor (tseries window test-part &optional log)
  (make-predictor tseries window
		  #'(lambda (pats)
		      (make-cascade-classifier pats nil
			'((:act-fn . :tanh) (:eps . 0.2) (:mu . 2.0) (:epochs . 200) (:thr . 1.e-5) (:recompute . 10)
			  (:verbosity . 2) (:max-err-bits . 0.0) (:misclassify-limit . 0.05))
			'((:eps . 1.0) (:mu . 2.0) (:epochs . 200) (:thr . 1.e-5) (:recompute . 10) (:verbosity . 0)
			  (:candidates . 10) (:hidden-num . 0) (:epochs-handicap . 10))))
		  test-part log))

(defun make-rcascade-predictor (tseries window test-part &optional log)
  (make-predictor tseries window
		  #'(lambda (pats)
		      (make-rcascade-classifier pats nil
			'((:act-fn . :tanh) (:eps . 0.2) (:mu . 2.0) (:epochs . 200) (:thr . 1.e-5) (:recompute . 10)
			  (:verbosity . 2) (:max-err-bits . 0.0) (:misclassify-limit . 0.05))
			'((:eps . 1.0) (:mu . 2.0) (:epochs . 200) (:thr . 1.e-5) (:recompute . 10) (:verbosity . 0)
			  (:candidates . 10) (:hidden-num . 30) (:epochs-handicap . 10))))
		  test-part log))

(defun make-adaboost-cascade-predictor (tseries window test-part &optional log)
  (make-predictor tseries window
		  #'(lambda (pats)
		      (make-adaboost-network pats
			#'(lambda (x)
			    (make-cascade-classifier x nil
			      '((:act-fn . :tanh) (:eps . 0.2) (:mu . 2.0) (:epochs . 200) (:thr . 1.e-5) (:recompute . 10)
				(:verbosity . 2) (:max-err-bits . 0.2) (:misclassify-limit . 0.1))
			      '((:eps . 1.0) (:mu . 2.0) (:epochs . 200) (:thr . 1.e-5) (:recompute . 10) (:verbosity . 0)
				(:candidates . 10) (:hidden-num . 3) (:epochs-handicap . 10))))
			'((:verbosity . t) (:max-weak . 3) (:misclassify-limit . 0.2))))
		  test-part log))

(defmethod eval-network ((network predictor) input)
  (let* ((latest-pat (or (pred-tmp-pat network) (pred-latest-pat network)))
	 (scale-bounds (pred-scale-bounds network))
	 (pat (first (shift-ts-pattern latest-pat #0m(-999) input)))
	 (window (pred-window network))
	 (enc-pat (ts-encode-inp pat latest-pat window scale-bounds (pred-log-p network)))
	 (enc-new (m*c (eval-network (pred-net network) (first enc-pat)) 1.01))
	 (new (m+ (m1 (undo-bound-scale-output-val (e1 enc-new) (svref scale-bounds 0)))
		  (second enc-pat))))
    (when (pred-log-p network) (smap-matrix-exp new))
    (setf (pred-tmp-pat network)
	  (list (first (shift-ts-pattern latest-pat new input))
		new))
    new))

(defun make-prediction-series (predictor infls)
  (let ((ts (make-instance 'tseries :values (make-matrix (length infls)))))
    (dotimes (i (length infls))
      (setf (aref (ts-values ts) i)
	    (e1 (eval-network predictor (elt infls i)))))
;			      (subseq (first (or (pred-tmp-pat predictor)
;						 (pred-latest-pat predictor)))
;				      (pred-window predictor))))))
    ts))
