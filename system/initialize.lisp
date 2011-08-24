(in-package :annil)

(export '(seed-weights nw-seed-weights))

(defun nw-seed-weights (weights &optional (input-range '(-1.0 1.0)))
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

(defun seed-weights (weights &optional (input-range '(-1.0 1.0)))
  (m*c (map-matrix weights #'(lambda (w) (declare (ignore w)) (apply #'plain-rng input-range))) 0.5))
