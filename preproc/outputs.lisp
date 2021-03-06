(in-package :annil)

;; bound scaling

(defun bound-scale-outputs-params-from-vec (vec interval &optional old-interval)
  (vector (coerce (aif (first old-interval) it (mmin vec)) 'single-float)
	  (coerce (aif (second old-interval) it (mmax vec)) 'single-float)
	  (coerce (first interval) 'single-float)
	  (coerce (second interval) 'single-float)))

(defun bound-scale-outputs-params (patterns interval &optional old-interval)
  (when (listp old-interval)
    (setf old-interval (make-array (patterns-output-dim patterns) :initial-element old-interval)))
  (map 'vector #'(lambda (vec int) (bound-scale-outputs-params-from-vec vec interval int))
       (patterns-dispatch (patterns)
			  (error "No outputs in these patterns")
			  (error "Can't get scaling params from single pattern")
			  (error "No outputs in these patterns")
			  (map 'vector #'(lambda (i)
					   (col (second patterns) i))
			       (iota (patterns-output-dim patterns)))
			  (error "No outputs in these patterns")
			  (map 'vector #'(lambda (i)
					   (map '(simple-array single-float (*))
						#'(lambda (x) (aref (second x) i)) patterns))
			       (iota (patterns-output-dim patterns))))
       old-interval))

(defun bound-scale-output-val (val params)
  (declare (optimize speed (safety 0)))
  (let ((x (%svref params 0))
	(nx (%svref params 2)))
    (%+ (%* (%- val x)
	    (%/ (%- (%svref params 3) nx)
		(%- (%svref params 1) x)))
	nx)))

(defun undo-bound-scale-output-val (val params)
  (declare (optimize speed (safety 0)))
  (let ((x (%svref params 0))
	(nx (%svref params 2)))
    (%+ (%* (%- val nx)
	    (%/ (%- (%svref params 1) x)
		(%- (%svref params 3) nx)))
	x)))

(defun bound-scale-pattern-output (output params)
  (dotimes (i (dim0 output))
    (setf (%fvref output i)
	  (bound-scale-output-val (%fvref output i)
				  (%ssvref params i))))
  output)

(defun bound-scale-patterns-outputs (patterns params)
  (patterns-dispatch (patterns)
		     (error "No outputs in these patterns")
		     (bound-scale-pattern-output (second patterns) params)
		     (error "No outputs in these patterns")
		     (dotimes (i (num-patterns patterns))
		       (setf (row (second patterns) i)
			     (bound-scale-pattern-output (row (second patterns) i) params)))
		     (error "No outputs in these patterns")
		     (map nil #'(lambda (x) (bound-scale-pattern-output (second x) params)) patterns))
  patterns)

(defun undo-bound-scale-pattern-output (output params)
  (declare (optimize speed (safety 0)))
  (dotimes (i (dim0 output))
    (setf (%fvref output i)
	  (undo-bound-scale-output-val (%fvref output i)
				       (%ssvref params i))))
  output)

(defun undo-bound-scale-patterns-outputs (patterns params)
  (patterns-dispatch (patterns)
		     (error "No outputs in these patterns")
		     (undo-bound-scale-pattern-output (second patterns) params)
		     (error "No outputs in these patterns")
		     (dotimes (i (num-patterns patterns))
		       (setf (row (second patterns) i)
			     (undo-bound-scale-pattern-output (row (second patterns) i) params)))
		     (error "No outputs in these patterns")
		     (map nil #'(lambda (x) (undo-bound-scale-pattern-output (second x) params)) patterns))
  patterns)

;; log scaling

(defun log-scale-patterns-outputs (patterns)
  (declare (optimize speed (safety 0)))
  (patterns-dispatch (patterns)
		     (error "No outputs in these patterns")
		     (let ((o (elt patterns 1)))
		       (assert (plusp (mmin o)) nil "Cannot log-scale non-positive values")
		       (smap-matrix-log o))
		     (error "No outputs in these patterns")
		     (let ((o (second patterns)))
		       (assert (plusp (mmin o)) nil "Cannot log-scale non-positive values")
		       (smap-matrix-log o))
		     (error "No outputs in these patterns")
		     (map nil #'(lambda (x) (let ((o (second x)))
					      (assert (plusp (mmin o)) nil "Cannot log-scale non-positive values")
					      (smap-matrix-log o)))
			  patterns))
  patterns)

(defun log-unscale-patterns-outputs (patterns)
  (declare (optimize speed (safety 0)))
  (patterns-dispatch (patterns)
		     (error "No outputs in these patterns")
		     (smap-matrix-exp (elt patterns 1))
		     (error "No outputs in these patterns")
		     (smap-matrix-exp (second patterns))
		     (error "No outputs in these patterns")
		     (map nil #'(lambda (x) (smap-matrix-exp (second x))) patterns))
  patterns)
