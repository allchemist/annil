(in-package :annil)

(defun scale-to-edges (vec edges &optional old-edges)
  (let ((nx (first edges))
	(ny (second edges))
	(x (if (null old-edges) (mmin vec) (first old-edges)))
	(y (if (null old-edges) (mmax vec) (second old-edges))))
    (map-matrix vec
		#'(lambda (val)
		    (+ (* (- val x) (/ (- ny nx) (- y x))) nx)))))

(defun patterns-scale-outputs (patterns ranges &optional old-ranges)
  (let (new-pat)
    (do-patterns (patterns p)
      (push (list (first p) (scale-to-edges (second p) ranges old-ranges)) new-pat))
    (nreverse new-pat)))

(defun autoscale-params (train-set)
  ;; get means and norms from train set
  ;; train set is an input matrix here

  (let ((dim0 (dim0 train-set))
	(dim1 (dim1 train-set)))
    (let ((col (make-matrix dim0))
	  (means (make-matrix dim1))
	  (norms (make-matrix dim1)))
      (dotimes (j dim1)
	(col train-set j col)
	(setf (aref means j) (mean col))
	;; *2 is a kludge
	;; real way of scaling: Xs=X*inv(diag(std(X)))
	(setf (aref norms j) (* (e-norm col) 2)))
      (list means norms))))

(defun autoscale-input (input means norms)
  (m/ (m- input means) norms))

(defun undo-autoscale-input (input means norms)
  (m+ (m* input norms) means))

(defun autoscale-patterns (patterns means norms)
  (etypecase patterns
    ((or matrix-inputs list-inputs)
       (do-patterns (patterns p)
	 (autoscale-input p means norms)))
    ((or matrix-patterns list-patterns)
       (do-patterns (patterns p)
	 (autoscale-patterns (first p) means norms)))))

(defun undo-autoscale-patterns (patterns means norms)
  (etypecase patterns
    ((or matrix-inputs list-inputs)
       (do-patterns (patterns p)
	 (undo-autoscale-input p means norms)))
    ((or matrix-patterns list-patterns)
       (do-patterns (patterns p)
	 (undo-autoscale-patterns (first p) means norms)))))
