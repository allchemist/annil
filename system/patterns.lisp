(in-package :annil)

(export '(single-pattern single-input matrix-input matrix-patterns svector-inputs svector-patterns
	  array-2d-p matrix-patterns-p svector-patterns-p svector-inputs-p single-pattern-p single-input-p
	  patterns-dispatch patterns-type get-pattern get-pattern-safe do-patterns do-patterns-safe do-patterns-shuffle
	  patterns-num patterns-input-dim patterns-output-dim convert-patterns-to-svector convert-patterns-to-matrix
	  store-patterns restore-patterns copy-patterns patterns-with-weights-p pattern-weight normalize-patterns-weights))

(declaim (inline array-2d-p matrix-patterns-p svector-patterns-p svector-inputs-p single-pattern-p single-input-p
		 pattern-weight %build-pattern))

;; patterns types:
;; matrix-inputs: simply 2d matrix
(defun array-2d-p (array)
  (and (arrayp array) (= (array-rank array) 2)))

;; matrix-patterns: list of two matrices: (inputs-matrix outputs-matrix)
(defun matrix-patterns-p (patterns)
  (and (array-2d-p (first patterns)) (array-2d-p (second patterns))))

;; simple-vector inputs
(defun svector-inputs-p (patterns)
  (typep patterns 'simple-vector))

;; simple-vector patterns
(defun svector-patterns-p (patterns)
  (and (typep patterns 'simple-vector)
       (listp (svref patterns 0))))

;; single-pattern: list of two vectors
(defun single-pattern-p (pattern)
  (and (listp pattern)
       (let ((len (length pattern)))
	 (or (= len 3)
	     (and (= len 2)
		  (vectorp (first pattern)))))))

(defun single-input-p (pattern)
  (typep pattern '(and vector (not simple-vector))))

(deftype single-pattern () '(satisfies single-pattern-p))     ; (# #)
(deftype single-input () '(satisfies single-input-p))         ; #

(deftype matrix-inputs () '(satisfies array-2d-p))            ; #2
(deftype matrix-patterns () '(satisfies matrix-patterns-p))   ; (#2 #2)
(deftype svector-inputs () '(satisfies svector-inputs-p))     ; #(# # ... )
(deftype svector-patterns () '(satisfies svector-patterns-p)) ; #((# #) ... )

(defmacro patterns-dispatch ((patterns)
			     single-input  single-pattern
			     matrix-inputs matrix-patterns
			     svector-inputs svector-patterns)
  `(cond ((svector-patterns-p ,patterns) ,svector-patterns)
	 ((svector-inputs-p  ,patterns) ,svector-inputs)
	 ((single-input-p    ,patterns) ,single-input)
	 ((array-2d-p        ,patterns) ,matrix-inputs)
	 ((single-pattern-p  ,patterns) ,single-pattern)
	 ((matrix-patterns-p ,patterns) ,matrix-patterns)))

(defun patterns-type (patterns)
  (patterns-dispatch (patterns)
		     'single-input   'single-pattern
		     'matrix-inputs  'matrix-patterns
		     'svector-inputs 'svector-patterns))

(defun patterns-with-weights-p (patterns)
  (patterns-dispatch (patterns)
		     nil (= (length patterns) 3)
		     nil (= (length patterns) 3)
		     nil (= (length (svref patterns 0)) 3)))

(defun %build-pattern (i o w)
  (if w
      (list i o w)
      (list i o)))

(defun pattern-weight (single-pattern)
  (when (= (length single-pattern) 3)
    (elt single-pattern 2)))

(defun get-pattern (patterns index)
  (patterns-dispatch (patterns)
		     (copy patterns)
		     (%build-pattern (first patterns)
				     (second patterns)
				     (pattern-weight patterns))
		     (row patterns index)
		     (%build-pattern (row (first patterns) index)
				     (row (second patterns) index)
				     (when (= (length patterns) 3)
				       (list (aref (third patterns) index))))
		     (copy (svref patterns index))
		     (let ((p (svref patterns index)))
		       (%build-pattern (first p) (second p) (pattern-weight p)))))

(defun get-pattern-safe (patterns index)
  (if (typep patterns 'simple-vector)
      (svref patterns index)
      (patterns-dispatch (patterns)
			 patterns
			 patterns
			 (row patterns index)
			 (%build-pattern (row (first patterns) index)
					 (row (second patterns) index)
					 (when (= (length patterns) 3)
					   (list (aref (third patterns) index))))
			 nil nil)))

(defun copy-patterns (patterns)
  (patterns-dispatch (patterns)
		     (copy patterns)
		     (%build-pattern (first patterns)
				     (second patterns)
				     (pattern-weight patterns))
		     (copy patterns)
		     (mapcar #'copy patterns)
		     (map 'simple-vector #'copy patterns)
		     (map 'simple-vector
			  (lambda (p) (%build-pattern (first p) (second p) (pattern-weight p)))
			  patterns)))

(defun normalize-patterns-weights (patterns)
  (patterns-dispatch (patterns)
		     (error "Weights normalization for single input makes no sense")
		     (error "Weights normalization for single pattern makes no sense")
		     (error "This patterns contain only inputs, weights normalization makes no sense")
		     (progn (m/c (third patterns) (mean (third patterns))) patterns)
		     (error "This patterns contain only inputs, weights normalization makes no sense")
		     (let ((sum 0.0))
		       (dotimes (i (length patterns))
			 (incf sum (elt (svref patterns i) 2)))
		       (dotimes (i (length patterns))
			 (let ((p (svref patterns i)))
			   (setf (elt p 2) (/ (elt p 2) sum))))
		       patterns)))

(defun num-patterns (patterns)
  (patterns-dispatch (patterns)
		     1 1
		     (dim0 patterns)
		     (dim0 (first patterns))
		     (length patterns)
		     (length patterns)))

(defun patterns-input-dim (patterns)
  (patterns-dispatch (patterns)
		     (dim0 patterns)
		     (dim0 (first patterns))
		     (dim1 patterns)
		     (dim1 (first patterns))
		     (dim0 (svref patterns 0))
		     (dim0 (first (svref patterns 0)))))

(defun patterns-output-dim (patterns)
  (patterns-dispatch (patterns)
		     (error "This patterns contain only inputs, output dimension makes no sense")
		     (dim0 (second patterns))
		     (error "This patterns contain only inputs, output dimension makes no sense")
		     (dim1 (second patterns))
		     (error "This patterns contain only inputs, output dimension makes no sense")
		     (dim0 (second (svref patterns 0)))))

(defun convert-patterns-to-svector (patterns)
  (patterns-dispatch (patterns)
		     (error "This is a single pattern, operation makes no sense")
		     (error "This is a single pattern, operation makes no sense")
		     (let* ((len (num-patterns patterns))
			    (new-inputs (make-array len)))
		       (dotimes (i len new-inputs)
			 (setf (svref new-inputs i)
			       (get-pattern patterns i))))
		     (let* ((len (num-patterns patterns))
			    (new-inputs (make-array len)))
		       (dotimes (i len new-inputs)
			 (setf (svref new-inputs i)
			       (get-pattern patterns i))))
		     (copy-patterns patterns)
		     (copy-patterns patterns)))

(defun convert-patterns-to-matrix (patterns)
   (patterns-dispatch (patterns)
		      (error "This is a single pattern, operation makes no sense")
		      (error "This is a single pattern, operation makes no sense")
		      (copy-patterns patterns)
		      (copy-patterns patterns)
		      (let ((new-inputs (make-matrix (list (num-patterns patterns)
							   (patterns-input-dim patterns)))))
			(dotimes (i (num-patterns patterns) new-inputs)
			  (setf (row new-inputs i) (get-pattern patterns i))))
		      (let* ((num-patterns (num-patterns patterns))
			     (weights-p (patterns-with-weights-p patterns))
			     (new-patterns
			      (cons (make-matrix (list num-patterns (patterns-input-dim patterns)))
				    (cons (make-matrix (list num-patterns (patterns-output-dim patterns)))
					  (when weights-p (list (make-matrix num-patterns)))))))
			(dotimes (i (num-patterns patterns) new-patterns)
			  (let ((pat (get-pattern patterns i)))
			    (setf (row (first new-patterns) i) (first pat))
			    (setf (row (second new-patterns) i) (second pat))
			    (when weights-p (setf (aref (third new-patterns) i) (third pat))))))))



(defmacro do-patterns ((patterns p) &body body)
  `(dotimes (i (num-patterns ,patterns))
     (let ((,p (get-pattern ,patterns i)))
       ,@body)))

(defmacro do-patterns-safe ((patterns p) &body body)
  `(dotimes (i (num-patterns ,patterns))
     (let ((,p (get-pattern-safe ,patterns i)))
       ,@body)))

(defmacro do-patterns-shuffle ((patterns p) &body body)
  `(let ((lst (random-shuffle-list (num-patterns ,patterns))))
     (dolist (i lst)
       (let ((,p (get-pattern ,patterns i)))
	 ,@body))))
