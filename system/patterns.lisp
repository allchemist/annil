(in-package :annil)

(export '(single-pattern single-input matrix-input matrix-patterns svector-inputs svector-patterns
	  array-2d-p matrix-patterns-p svector-patterns-p svector-inputs-p single-pattern-p single-input-p
	  patterns-dispatch patterns-type get-pattern get-pattern-safe do-patterns do-patterns-safe do-patterns-shuffle
	  num-patterns patterns-input-dim patterns-output-dim convert-patterns-to-svector convert-patterns-to-matrix
	  copy-patterns patterns-with-weights-p pattern-weight normalize-patterns-weights patterns-params-num))

(declaim (inline array-2d-p matrix-patterns-p svector-patterns-p svector-inputs-p single-pattern-p single-input-p pattern-weight))

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
	 (and (vectorp (first pattern))
	      (or (= len 3) (= len 2))))))

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
	 ((matrix-patterns-p ,patterns) ,matrix-patterns)
	 (t (error "Unknown type of patterns"))))

(defun patterns-type (patterns)
  (patterns-dispatch (patterns)
		     'single-input   'single-pattern
		     'matrix-inputs  'matrix-patterns
		     'svector-inputs 'svector-patterns))

(defun patterns-only-inputs (patterns)
  (patterns-dispatch (patterns) t nil t nil t nil))

(defun patterns-with-goals (patterns)
  (patterns-dispatch (patterns) nil t nil t nil t))

(defun patterns-with-weights-p (patterns)
  (patterns-dispatch (patterns)
		     nil (= (length patterns) 3)
		     nil (= (length patterns) 3)
		     nil (= (length (svref patterns 0)) 3)))

(defun pattern-weight (single-pattern)
  (if (= (length single-pattern) 3)
      (aref (elt single-pattern 2) 0)
      1.0))

(defun (setf pattern-weight) (value single-pattern)
  (setf (aref (elt single-pattern 2) 0) value))

(defun get-pattern (patterns index)
  (assert (and (not (minusp index)) (< index (num-patterns patterns)))
	  nil "Provided index is negative or more than possible")
  (patterns-dispatch (patterns)
		     (copy patterns)
		     (mapcar #'copy patterns)
		     (row patterns index)
		     (mapcar #'(lambda (x) (row x index)) patterns)
		     (copy (svref patterns index))
		     (mapcar #'copy (svref patterns index))))

(defun get-pattern-safe (patterns index)
  (if (typep patterns 'simple-vector)
      (svref patterns index)
      (patterns-dispatch (patterns)
			 patterns
			 patterns
			 (row patterns index)
			 (mapcar #'(lambda (x) (row x index)) patterns)
			 nil nil)))

(defun copy-patterns (patterns)
  (patterns-dispatch (patterns)
		     (copy patterns)
		     (mapcar #'copy patterns)
		     (copy patterns)
		     (mapcar #'copy patterns)
		     (map 'simple-vector #'copy patterns)
		     (map 'simple-vector #'(lambda (p) (mapcar #'copy p)) patterns)))

(defun normalize-patterns-weights (patterns)
  (patterns-dispatch (patterns)
		     (error "Weights normalization for single input makes no sense")
		     (error "Weights normalization for single pattern makes no sense")
		     (error "This patterns contain only inputs, weights normalization makes no sense")
		     (progn (setf (col (third patterns) 0)
				  (m/c (col (third patterns) 0) (msum (third patterns))))
			    patterns)
		     (error "This patterns contain only inputs, weights normalization makes no sense")
		     (let ((sum 0.0))
		       (dotimes (i (length patterns))
			 (incf sum (pattern-weight (svref patterns i))))
		       (dotimes (i (length patterns))
			 (let ((p (svref patterns i)))
			   (setf (pattern-weight p) (/ (pattern-weight p) sum))))
		       patterns)))

(defun num-patterns (patterns)
  (tthe fixnum (if (null patterns) 0
		   (patterns-dispatch (patterns)
				      1 1
				      (dim0 patterns)
				      (dim0 (first patterns))
				      (length patterns)
				      (length patterns)))))

(defun patterns-input-dim (patterns)
  (tthe fixnum
	(patterns-dispatch (patterns)
			   (dim0 patterns)
			   (dim0 (first patterns))
			   (dim1 patterns)
			   (dim1 (first patterns))
			   (dim0 (svref patterns 0))
			   (dim0 (first (svref patterns 0))))))

(defun patterns-output-dim (patterns)
  (tthe fixnum
	(patterns-dispatch (patterns)
			   (error "This patterns contain only inputs, output dimension makes no sense")
			   (dim0 (second patterns))
			   (error "This patterns contain only inputs, output dimension makes no sense")
			   (dim1 (second patterns))
			   (error "This patterns contain only inputs, output dimension makes no sense")
			   (dim0 (second (svref patterns 0))))))

(defun patterns-params-num (patterns)
  (tthe fixnum
	(patterns-dispatch (patterns)
			   (error "This patterns contain only inputs, output dimension makes no sense")
			   (dim0 (third patterns))
			   (error "This patterns contain only inputs, output dimension makes no sense")
			   (dim1 (third patterns))
			   (error "This patterns contain only inputs, output dimension makes no sense")
			   (dim0 (third (svref patterns 0))))))

(defun convert-patterns-to-svector (patterns)
  (tthe simple-vector
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
			   (copy-patterns patterns))))

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
					  (when weights-p (list (make-matrix (list num-patterns (patterns-params-num patterns)))))))))
			(dotimes (i (num-patterns patterns) new-patterns)
			  (let ((pat (get-pattern patterns i)))
			    (setf (row (first new-patterns) i) (first pat))
			    (setf (row (second new-patterns) i) (second pat))
			    (when weights-p (setf (row (third new-patterns) i) (third pat))))))))

(defmacro do-patterns ((patterns p &optional i) &body body)
  (let ((idx (or i (gensym))))
    `(dotimes (,idx (num-patterns ,patterns))
       (let ((,p (get-pattern ,patterns ,idx)))
	 ,@body))))

(defmacro do-patterns-shuffle ((patterns p &optional i) &body body)
  (let ((idx (or i (gensym))))
    `(let ((lst (random-shuffle-list (num-patterns ,patterns))))
       (dolist (,idx lst)
	 (let ((,p (get-pattern ,patterns ,idx)))
	   ,@body)))))

(defmacro do-patterns-safe ((patterns p &optional i) &body body)
  `(if (vectorp ,patterns)
       (do-patterns-safe/sv (,patterns ,p ,i) ,@body)
       (do-patterns (,patterns ,p ,i) ,@body)))

(defmacro do-patterns-safe/sv ((patterns p &optional i) &body body &environment env)
  (let ((idx (or i (gensym))))
    `(locally (declare (optimize speed (safety 0))
		       (type simple-vector ,patterns))
       (dotimes (,idx (tthe fixnum (length ,patterns)))
	 (let ((,p (svref ,patterns ,idx)))
	   (locally (declare (optimize ,@(sb-cltl2:declaration-information 'optimize env)))
	     ,@body))))))
