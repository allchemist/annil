(in-package :annil)

(export '(single-pattern single-input matrix-input matrix-patterns svector-inputs svector-patterns
	  array-2d-p matrix-patterns-p svector-patterns-p svector-inputs-p single-pattern-p single-input-p
	  patterns-dispatch patterns-type get-pattern get-pattern-safe do-patterns do-patterns-safe do-patterns-shuffle
	  patterns-num patterns-input-dim patterns-output-dim convert-patterns-to-svector convert-patterns-to-matrix
	  store-patterns restore-patterns copy-patterns))

(declaim (inline array-2d-p matrix-patterns-p svector-patterns-p svector-inputs-p single-pattern-p single-input-p))

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
       (= (length pattern) 2)
       (vectorp (first pattern))))

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
(defun get-pattern (patterns index)
  (patterns-dispatch (patterns)
		     (copy patterns)
		     (mapcar #'copy patterns)
		     (row patterns index)
		     (list (row (first patterns) index)
			   (row (second patterns) index))
		     (copy (svref patterns index))
		     (map 'list #'copy (svref patterns index))))

(defun get-pattern-safe (patterns index)
  (if (svector-patterns-p patterns)
      (svref patterns index)
      (patterns-dispatch (patterns)
			 patterns
			 patterns
			 (row patterns index)
			 (list (row (first patterns) index)
			       (row (second patterns) index))
			 (svref patterns index)
			 (svref patterns index))))

(defun copy-patterns (patterns)
  (patterns-dispatch (patterns)
		     (copy patterns)
		     (mapcar #'copy patterns)
		     (copy patterns)
		     (mapcar #'copy patterns)
		     (map 'vector #'copy patterns)
		     (map 'vector #'(lambda (p) (mapcar #'copy p)) patterns)))

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



(defun store-patterns (patterns path)
  (with-open-file (s path
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
    (princ "annil patterns storage" s)
    (patterns-dispatch (patterns)
		       (progn (print "single-input")
			      (store-matrix patterns s t))
		       (progn (print "single-pattern")
			      (store-matrix (first patterns) s t)
			      (store-matrix (second patterns) s t))
		       (progn (print "matrix-inputs")
			      (store-matrix patterns s t))
		       (progn (print "matrix-patterns")
			      (store-matrix patterns s t)
			      (store-matrix patterns s t))
		       (progn (print "list-inputs" s)
			      (print (num-patterns patterns) s)
			      (dotimes (i (length patterns))
				(store-matrix (svref patterns i) s t)))
		       (progn (print "list-patterns" s)
			      (print (num-patterns patterns) s)
			      (dotimes (i (num-patterns patterns))
				(let ((p (svref patterns i)))
				  (store-matrix (first p) s t)
				  (store-matrix (second p) s t)))))))

(defun restore-patterns (path)
  (with-open-file (s path)
    (assert (string= (read-line s) "annil patterns storage") nil "Not a patterns storage")
    (let ((type (read-line s)))
      (cond ((string= type "single-input")
	     (restore-matrix s))
	    ((string= type "single-pattern")
	     (list (restore-matrix s)
		   (restore-matrix s)))
	    ((string= type "matrix-inputs")
	     (restore-matrix s))
	    ((string= type "matrix-patterns")
	     (list (restore-matrix s)
		   (restore-matrix s)))
	    ((string= type "list-inputs")
	     (let* ((len (read s))
		    (pats (make-array len)))
	       (dotimes (i len pats)
		 (setf (svref pats i) (restore-matrix s)))))
	    ((string= type "list-patterns")
	     (let* ((len (read s))
		    (pats (make-array len)))
	       (dotimes (i len pats)
		 (setf (svref pats i) (list (restore-matrix s) (restore-matrix s))))))
	    (t (error "Unknown patterns type"))))))
