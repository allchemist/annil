(in-package :annil)

(declaim (inline array-2d-p matrix-patterns-p list-patterns-p list-inputs-p simgle-patterns-p single-input-p))
;; patterns types:
;; matrix-inputs: simply 2d matrix
(defun array-2d-p (array)
  (and (arrayp array) (= (array-rank array) 2)))

;; matrix-patterns: list of two matrices: (inputs-matrix outputs-matrix)
(defun matrix-patterns-p (patterns)
  (and (array-2d-p (first patterns)) (array-2d-p (second patterns))))

;; list-patterns: list of sublists.
;; each sublist is a list of a single input and corresponding output as vectors.
(defun list-patterns-p (patterns)
  (listp (first patterns)))

;; list-inputs: list of vectors. each vector is an only-input pattern
(defun list-inputs-p (patterns)
  (and (listp patterns)
       (vectorp (first patterns))))

;; single-pattern: list of two vectors
(defun single-pattern-p (pattern)
  (and (listp pattern)
       (= (length pattern) 2)
       (vectorp (first pattern))))

(defun single-input-p (pattern)
  (vectorp pattern))

(deftype single-pattern () '(satisfies single-pattern-p))    ; (# #)
(deftype single-input () '(satisfies single-input-p))        ; #

(deftype matrix-inputs () '(satisfies array-2d-p))           ; #2
(deftype matrix-patterns () '(satisfies matrix-patterns-p))  ; (#2 #2)
(deftype list-inputs () '(satisfies list-inputs-p))          ; (# # ... )
(deftype list-patterns () '(satisfies list-patterns-p))      ; ((# #) ... )

(defmacro patterns-dispatch ((patterns)
			     single-input  single-pattern
			     matrix-inputs matrix-patterns
			     list-inputs   list-patterns)
  `(cond ((single-input-p    ,patterns) ,single-input)
	 ((array-2d-p        ,patterns) ,matrix-inputs)
	 ((single-pattern-p  ,patterns) ,single-pattern)
	 ((list-inputs-p     ,patterns) ,list-inputs)
	 ((matrix-patterns-p ,patterns) ,matrix-patterns)
	 ((list-patterns-p   ,patterns) ,list-patterns)))

(defun patterns-type (patterns)
  (patterns-dispatch (patterns)
		     'single-input  'single-pattern
		     'matrix-inputs 'matrix-patterns
		     'list-inputs   'list-patterns))

(defun get-pattern (patterns index)
  (patterns-dispatch (patterns)
		     (copy patterns)
		     (mapcar #'copy patterns)
		     (row patterns index)
		     (list (row (first patterns) index)
			   (row (second patterns) index))
		     (copy (elt patterns index))
		     (mapcar #'copy (elt patterns index))))

(defun get-pattern-safe (patterns index)
  (if (list-patterns-p patterns)
      (elt patterns index)
      (patterns-dispatch (patterns)
			 patterns
			 patterns
			 (row patterns index)
			 (list (row (first patterns) index)
			       (row (second patterns) index))
			 (elt patterns index)
			 (elt patterns index))))

(defun copy-patterns (patterns)
  (patterns-dispatch (patterns)
		     (copy patterns)
		     (mapcar #'copy patterns)
		     (copy patterns)
		     (mapcar #'copy patterns)
		     (mapcar #'copy patterns)
		     (mapcar #'(lambda (p) (mapcar #'copy p)) patterns)))

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
		     (dim0 (first patterns))
		     (dim0 (first (first patterns)))))

(defun patterns-output-dim (patterns)
  (patterns-dispatch (patterns)
		     (error "This patterns contain only inputs, output dimension makes no sense")
		     (dim0 (second patterns))
		     (error "This patterns contain only inputs, output dimension makes no sense")
		     (dim1 (second patterns))
		     (error "This patterns contain only inputs, output dimension makes no sense")
		     (dim0 (second (first patterns)))))

(defun convert-patterns-to-list (patterns)
  (patterns-dispatch (patterns)
		     (error "This is a single pattern, operation makes no sense")
		     (error "This is a single pattern, operation makes no sense")
		     (let ((new-inputs nil))
		       (dotimes (i (num-patterns patterns))
			 (push (get-pattern patterns i) new-inputs))
		       (nreverse new-inputs))
		     (let ((new-patterns nil))
		       (dotimes (i (num-patterns patterns))
			 (push (get-pattern patterns i) new-patterns))
		       (nreverse new-patterns))
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
		      (let ((new-patterns
			     (list (make-matrix (list (num-patterns patterns)
						      (patterns-input-dim patterns)))
				   (make-matrix (list (num-patterns patterns)
						      (patterns-output-dim patterns))))))
			(dotimes (i (num-patterns patterns) new-patterns)
			  (let ((pat (get-pattern patterns i)))
			    (setf (row (first new-patterns) i) (first pat))
			    (setf (row (second new-patterns) i) (second pat)))))))

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

(defun pattern-error (output-fn pattern)
  (msum (map-matrix-square (m- (funcall output-fn (first pattern)) (second pattern)))))

(defun patterns-error (output-fn patterns)
  (let ((sum 0))
    (do-patterns (patterns p)
      (incf sum (pattern-error output-fn p)))
    (/ sum (num-patterns patterns))))

