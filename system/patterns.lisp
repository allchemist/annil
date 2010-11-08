(in-package :annil)

;; pattern types

;; patterns can be input-output pairs, or simply inputs.
;; also two representations of each type: matrix and list.
;; when matrix is used, patterns are stored in rows

;; matrix-patterns: list of two matrices: (inputs-matrix outputs-matrix)
(defun matrix-patterns-p (patterns)
  (and (arrayp (first patterns)) (arrayp (second patterns))))

;; list-patterns: list of sublists.
;; each sublist is a list of a single input and corresponding output as vectors.
(defun list-patterns-p (patterns)
  (listp (first patterns)))

;; list-inputs: list of vectors. each vector is an only-input pattern
(defun list-inputs-p (patterns)
  (and (listp patterns)
       (vectorp (first patterns))))

(defun single-pattern-p (pattern)
  (or (vectorp pattern)
      (and (listp pattern)
	   (vectorp (first pattern)))))

(deftype matrix-patterns () '(satisfies matrix-patterns-p))
;; matrix inputs are simply a 2d-array
(deftype matrix-inputs () '(satisfies arrayp))
(deftype list-patterns () '(satisfies list-patterns-p))
(deftype list-inputs () '(satisfies list-inputs-p))

;; return a pair of input-output vectors or single input

#|
;; does not work with current version of sb-math

;; extracting patterns from matrix representation does no consing.
;; a displaced row is returned.
;; so every change on extracted patterns modifies the source matrix
(defun get-pattern (patterns index)
  (etypecase patterns
    (matrix-inputs (row-bind patterns index))
    (matrix-patterns (list (row-bind (first patterns) index)
			   (row-bind (second patterns) index)))
    (list-inputs (elt patterns index))
    (list-patterns (elt patterns index))))
|#

;; get-pattern returnes a copy of a pattern, 
;; so it could be modified separately
(defun get-pattern (patterns index)
  (etypecase patterns
    (matrix-inputs (row patterns index))
    (matrix-patterns (list (row (first patterns) index)
			   (row (second patterns) index)))
    (list-inputs (copy (elt patterns index)))
    (list-patterns (mapcar #'copy (elt patterns index)))))

;; return a copy of whole patterns
(defun copy-patterns (patterns)
  (etypecase patterns
    (matrix-inputs (copy patterns))
    (matrix-patterns (list (copy (first patterns))
			   (copy (second patterns))))
    (list-inputs (mapcar #'copy patterns))
    (list-patterns (mapcar #'(lambda (x) (list (copy (first x)) (copy (second x))))
			   patterns))))

;; count patterns
(defun num-patterns (patterns)
  (etypecase patterns
    (matrix-inputs (dim0 patterns))
    (matrix-patterns
       (let ((num (dim0 (first patterns))))
	 (assert (= num (dim0 (second patterns)))
		 nil "Unbalanced inputs and outputs number in patterns")
	 num))
    (list-inputs (length patterns))
    (list-patterns (length patterns))))

;; dimension of input
(defun patterns-input-dim (patterns)
  (etypecase patterns
    (matrix-inputs (dim1 patterns))
    (matrix-patterns (dim1 (first patterns)))
    (list-inputs (dim0 (first patterns)))
    (list-patterns (dim0 (first (first patterns))))))

;; dimension of output.
(defun patterns-output-dim (patterns)
  (etypecase patterns
    (matrix-patterns (dim1 (second patterns)))
    (list-patterns (dim0 (second (first patterns))))
    (matrix-inputs (error "This patterns contain only inputs, output dimension makes no sense"))
    (list-inputs (error "This patterns contain only inputs, output dimension makes no sense"))))

;; converting patterns

;; if patterns are matrix-patterns, convert to list-patterns
;; else do nothing
;; a copy is returned in each case
(defun convert-patterns-to-list (patterns)
  (etypecase patterns
    (list-patterns (copy-patterns patterns))
    (list-inputs (copy-patterns patterns))
    (matrix-inputs
       (let ((new-inputs nil))
	 (dotimes (i (num-patterns patterns))
	   (push (get-pattern patterns i) new-inputs))
	 (nreverse new-inputs)))
    (matrix-patterns
       (let ((new-patterns nil))
	 (dotimes (i (num-patterns patterns))
	   (push (get-pattern patterns i) new-patterns))
	 (nreverse new-patterns)))))

;; if patterns are list-patterns, convert to matrix-patterns
;; else do nothing
;; a copy is returned in each case
(defun convert-patterns-to-matrix (patterns)
  (etypecase patterns
    (matrix-patterns (copy-patterns patterns))
    (matrix-inputs (copy-patterns patterns))
    (list-inputs
       (let ((new-inputs (make-matrix (list (num-patterns patterns)
					    (patterns-input-dim patterns)))))
	 (dotimes (i (num-patterns patterns) new-inputs)
	   (setf (row new-inputs i) (get-pattern patterns i)))))
	 
    (list-patterns
       (let ((new-patterns
	      (list (make-matrix (list (num-patterns patterns)
				       (patterns-input-dim patterns)))
		    (make-matrix (list (num-patterns patterns)
				       (patterns-output-dim patterns))))))
	 (dotimes (i (num-patterns patterns) new-patterns)
	   (let ((pat (get-pattern patterns i)))
	     (setf (row (first new-patterns) i) (first pat))
	     (setf (row (second new-patterns) i) (second pat))))))))

;; iteration through patterns
;; not copying
(defmacro do-patterns ((patterns &optional p) &body body)
  (let ((pat (or p (gensym))))
    `(dotimes (i (num-patterns ,patterns))
       (let ((,pat (get-pattern ,patterns i)))
	 ,@body))))

(defmacro do-patterns-shuffle ((patterns &optional p) &body body)
  (let ((pat (or p (gensym))))
    `(let ((lst (random-shuffle-list (num-patterns ,patterns))))
       (dolist (i lst)
	 (let ((,pat (get-pattern ,patterns i)))
	   ,@body)))))

;; count error on patterns

(defun pattern-error (output-fn pattern)
  (msum (map-matrix-square (m- (funcall output-fn (first pattern)) (second pattern)))))

(defun patterns-error (output-fn patterns)
  (let ((sum 0))
    (do-patterns (patterns p)
      (incf sum (pattern-error output-fn p)))
    (/ sum (num-patterns patterns))))

;; splitting


(defun split-patterns-by-class (patterns class-size part)
  (let (class1 class2)
    (dotimes (class (/ (num-patterns patterns) class-size))
      (dotimes (i class-size)
	(let ((p (get-pattern patterns (+ (* class class-size) i))))
	  (if (<= (/ i class-size) part)
	      (push p class1)
	      (push p class2)))))
    (list class1 class2)))
