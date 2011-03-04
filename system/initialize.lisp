(in-package :annil)

(defun linspace (xmin xmax points)
  (let* ((vec (make-matrix points))
	 (type (array-element-type vec)))
    (loop for idx from 0 below points
	  for val from xmin to xmax by (/ (- xmax xmin) (1- points))
	  do (setf (aref vec idx) (coerce val type)))
    vec))

(defun initialize-weights (weights input-range)
  (let ((H (dim0 weights))
	(I (dim1 weights)))
    (let ((w-mag (* 0.7 (expt H (/ I))))
	  (b-linspace (when (> H 1) (linspace -1 1 H))))
      (flet ((norm-row (iv)
	       (setf (row weights iv) (normalize (row weights iv)))))
	(dotimes (ii H)
	  (dotimes (jj (1- I))
	    (setf (aref weights ii jj)
		  (* w-mag
		     (apply #'plain-rng input-range)))))
	(dotimes (ii H)
	  (setf (aref weights ii (1- I))
		(if (= H 1)
		    (random-value (abs (aref weights ii 0)))
		    (* (signum (aref weights ii 0))
		       w-mag
		       (aref b-linspace ii))))))))
  weights)

