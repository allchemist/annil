(in-package :annil)

(export '(autocorrelogramm RS diff-tseries log-tseries undo-log-tseries scale-tseries-params scale-tseries))

;; autocorrelation

(defun %autocorrelogramm (tseries nlags)
  (let* ((values (ts-values tseries))
	 (N (length values))
	 (mean (mean values))
	 (Co (* (variance values :mean mean) N)))
    (flet ((Ch (lag)
	     (declare (optimize speed (safety 0)))
	     (let ((sum 0.0))
	       (%dotimes (i (- N lag))
		 (%incf sum (%* (%- (%fvref values i) mean)
				(%- (%fvref values (+ i lag)) mean))))
	       sum)))
      (make-instance 'tseries
		     :values (collect-sf (i nlags) (/ (Ch (1+ i)) Co))
		     :influences (ts-influences tseries)))))

(defun autocorrelogramm (tseries nlags)
  (ts-dispatch tseries
	       (%autocorrelogramm tseries nlags)
	       (discont-tseries-op tseries #'(lambda (ts) (%autocorrelogramm ts nlags)))))

;; RS analisys

(defun %RS (tseries)
  (let* ((X (ts-values tseries))
	 (mean (mean X))
	 (N (length X))
	 (Y (m-c (copy X) mean))
	 (Z (collect-sf (i N) (msum Y :end (1+ i))))
	 (R (collect-sf (i N)
	      (- (mmax Z :end (1+ i))
		 (mmin Z :end (1+ i)))))
	 (S (collect-sf (i N)
	      (if (<= i 1) 0.0
		  (sqrt (* (variance X :end (1+ i)) (/ i (1+ i))))))))
    (setf (aref S 0) 1.0
	  (aref S 1) 2.0)
    (make-instance 'tseries :values (m/ R S) :influences (ts-influences tseries))))


(defun RS (tseries)
  (ts-dispatch tseries
	       (%RS tseries)
	       (discont-tseries-op tseries #'%RS)))

;; differencies

(defun %%diff-tseries (tseries)
  (let ((vals (ts-values tseries)))
    (make-instance 'tseries
		   :values (collect-sf (i (1- (length vals)))
			     (- (%fvref vals (1+ i)) (%fvref vals i)))
		   :influences (ts-influences tseries))))

(defun %%n-diff-tseries (tseries order)
  (assert (>= order 0) nil "Differencing order should be positive integer")
  (if (zerop order) tseries
      (%%n-diff-tseries (%%diff-tseries tseries) (1- order))))

(defun %diff-tseries (tseries order)
  (if order
      (%%n-diff-tseries tseries order)
      (%%diff-tseries tseries)))

(defun diff-tseries (tseries &optional order)
  (ts-dispatch tseries
	       (%diff-tseries tseries order)
	       (discont-tseries-op tseries #'(lambda (ts) (%diff-tseries ts order)))))

;; log

(defun %log-tseries (tseries)
  (make-instance 'tseries
		 :values (smap-matrix-log (copy (ts-values tseries)))
		 :influences (ts-influences tseries)))

(defun log-tseries (tseries)
  (ts-dispatch tseries
	       (%log-tseries tseries)
	       (discont-tseries-op tseries #'%log-tseries)))


(defun %undo-log-tseries (tseries)
  (make-instance 'tseries
		 :values (smap-matrix-exp (copy (ts-values tseries)))
		 :influences (ts-influences tseries)))


(defun undo-log-tseries (tseries)
  (ts-dispatch tseries
	       (%undo-log-tseries tseries)
	       (discont-tseries-op tseries #'%undo-log-tseries)))

;; scale

(defun %scale-tseries-params (tseries)
  (let ((infs (ts-influences tseries)))
    (apply #'concatenate 'vector
	   (vector (bound-scale-outputs-params-from-vec (ts-values tseries) '(-1.0 1.0)))
	   (loop for i from 0 below (length (svref infs 0))
		 collect (list (bound-scale-outputs-params-from-vec
				(map 'sf-seq #'(lambda (x) (aref x i)) infs)
				'(-1.0 1.0)))))))

(defun %n-scale-tseries-params (tseries)
  (let* ((params (mapcar #'scale-tseries-params (dts-parts tseries)))
	 (new (make-array (length (first params)))))
    (dotimes (i (length new))
      (let* ((par (map 'vector #'(lambda (x) (elt x i)) params))
	     (min (mmin (map 'sf-seq #'(lambda (x) (aref x 0)) par)))
	     (max (mmax (map 'sf-seq #'(lambda (x) (aref x 1)) par))))
	(setf (svref new i) (vector min max -1.0 1.0))))
    new))

(defun scale-tseries-params (tseries)
  (ts-dispatch tseries
	       (%scale-tseries-params tseries)
	       (%n-scale-tseries-params tseries)))

(defun %scale-tseries (tseries params)
  (let ((inf-params (subseq params 1)))
    (make-instance 'tseries
		   :values (map-matrix (copy (ts-values tseries))
				       #'(lambda (x)
					   (bound-scale-output-val x (svref params 0))))
		   :influences (map 'vector #'(lambda (x)
						(bound-scale-pattern-output (copy x) inf-params))
				    (ts-influences tseries)))))

(defun scale-tseries (tseries &optional params)
  (unless params (setf params (scale-tseries-params tseries)))
  (values (ts-dispatch tseries
		       (%scale-tseries tseries params)
		       (discont-tseries-op tseries #'(lambda (ts) (%scale-tseries ts params))))
	  params))
