(in-package :annil)

(export '(tseries ts-values ts-influences make-tseries generate-tseries
	  discont-tseries dts-parts dts-skips plot-tseries))

(defclass tseries ()
  ((values :initarg :values :accessor ts-values)
   (influences :initarg :influences :accessor ts-influences)))

(defun make-tseries (patterns)
  (make-instance 'tseries
		 :values (map 'sf-seq #'e1 patterns)
		 :influences (when (/= (length patterns) 1)
			       (map 'vector #'(lambda (x) (subseq x 1)) patterns))))

(defun generate-tseries (generator linspace-params)
  (make-tseries
   (map 'vector #'(lambda (x)
		    (let ((v (funcall generator x)))
		      (if (numberp v) (m1 v) v)))
	(apply #'linspace linspace-params))))

(defun %plot-tseries (tseries tseries1)
  (let ((values (ts-values tseries))
	(values1 (when tseries1 (ts-values tseries1))))
    (when tseries1 (assert (= (length values1) (length values)) nil "Cannot plot two series of different length"))
    (gplt:make-plot
     (let ((t1 (loop for i from 0 below (length values)
		     collect (list i (aref values i)))))
       (if tseries1
	   (list t1 (loop for i from 0 below (length values1)
			  collect (list i (aref values1 i))))
	   t1))
     :multi (when tseries1 '('red 'blue)) :with :lines
     :ranges `((0 ,(length values))
	       (,(min (mmin values) (mmin values1))
		,(max (mmax values) (mmax values1)))))))

(defclass discont-tseries ()
  ((parts :initarg :parts :accessor dts-parts)
   (skips :initarg :skips :accessor dts-skips)))

(defun %plot-discont-tseries (tseries tseries1)
  (assert (not tseries1) nil "Plotting two discontinous series not implemented")
  (let ((parts (dts-parts tseries))
	(skips (dts-skips tseries))
	(off 0)
	(max-vals nil)
	(min-vals nil))
    (gplt:make-plot
     (append
      (loop for p from 0 below (length skips) collect
	(let ((vals (ts-values (elt parts p))))
	  (prog1
	      (loop for i from 0 below (length vals) collect
		(list (+ off i) (aref vals i)))
	    (push (mmax vals) max-vals)
	    (push (mmin vals) min-vals)
	    (incf off (length vals))
	    (incf off (elt skips p)))))
      (let ((vals (ts-values (elt parts (length skips)))))
	(prog1
	    (list (loop for i from 0 below (length vals) collect
	      (list (+ off i) (aref vals i))))
	  (push (mmax vals) max-vals)
	  (push (mmin vals) min-vals)
	  (incf off (length vals)))))
     :with :lines :multi t :ranges `((0 ,off) (,(apply #'min min-vals) ,(apply #'max max-vals))))))

(defmacro ts-dispatch (ts single-op discont-op)
  `(ecase (class-name (class-of ,ts))
     (tseries ,single-op)
     (discont-tseries ,discont-op)))

(defun plot-tseries (tseries &optional tseries1)
  (ts-dispatch tseries
	       (%plot-tseries tseries tseries1)
	       (%plot-discont-tseries tseries tseries1)))

(defun discont-tseries-op (tseries operation)
  (make-instance 'discont-tseries
		 :parts (mapcar operation (dts-parts tseries))
		 :skips (dts-skips tseries)))
