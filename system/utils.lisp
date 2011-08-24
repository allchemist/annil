(in-package :annil)

(export '(info activation param annil-relative iota collect-sf append-sf m1 e1 sf-seq))

(defmacro info (&rest body)
  `(progn (format *standard-output* " ;; ")
	  (format *standard-output* ,@body)))

;; activation functions

(defmacro define-single-float-mapper (name)
  (subseq (macroexpand (second (macroexpand `(define-mapping-function ,name :annil)))) 0 4))

(defun activation (obj fun)
  (ecase fun
    (:sigmoid (sigmoid-fn obj))
    (:asigmoid (asigmoid-fn obj))
    (:tanh (tanh-fn obj))
    (:linear obj)))

(defun activation-deriv (obj fun)
  (ecase fun
    (:sigmoid (sigmoid-fn-deriv obj))
    (:asigmoid (asigmoid-fn-deriv obj))
    (:tanh (tanh-fn-deriv obj))
    (:linear (linear-fn-deriv obj))))

;;sigmoid

(defun sigmoid-fn (x)
  (etypecase x
    (single-float (%sigmoid-fn x))
    (number (%sigmoid-fn (coerce x 'single-float)))
    (array (smap-matrix-%sigmoid-fn x))))

(defun sigmoid-fn-deriv (x)
  (etypecase x
    (single-float (%sigmoid-fn-deriv x))
    (number (%sigmoid-fn-deriv (coerce x 'single-float)))
    (array (smap-matrix-%sigmoid-fn-deriv x))))

(defun %sigmoid-fn (x)
  (declare (optimize speed (safety 0))
	   (type single-float x))
  (tthe single-float
	(cond ((< x -15.0) -0.5)
	      ((> x 15.0) +0.5)
	      (t (- (/ 1.0 (%+ 1.0 (exp (- x)))) 0.5)))))

(defun %sigmoid-fn-deriv (fn)
  (declare (optimize speed (safety 0))
	   (type single-float fn))
  (tthe single-float (- 0.24 (* fn fn))))

(define-single-float-mapper %sigmoid-fn)
(define-single-float-mapper %sigmoid-fn-deriv)

;; asigmoid

(defun asigmoid-fn (x)
  (etypecase x
    (single-float (%asigmoid-fn x))
    (number (%asigmoid-fn (coerce x 'single-float)))
    (array (smap-matrix-%asigmoid-fn x))))

(defun asigmoid-fn-deriv (x)
  (etypecase x
    (single-float (%asigmoid-fn-deriv x))
    (number (%asigmoid-fn-deriv (coerce x 'single-float)))
    (array (smap-matrix-%asigmoid-fn-deriv x))))

(defun %asigmoid-fn (x)
  (declare (optimize speed (safety 0))
	   (type single-float x))
  (tthe single-float (cond ((< x -15.0) 0.0)
			   ((> x 15.0) 1.0)
			   (t (/ 1.0 (%+ 1.0 (exp (- x))))))))

(defun %asigmoid-fn-deriv (fn)
  (declare (optimize speed (safety 0))
	   (type single-float fn))
  (tthe single-float (+ (* fn (- 1.0 fn)) 0.01)))

(define-single-float-mapper %asigmoid-fn)
(define-single-float-mapper %asigmoid-fn-deriv)

;; tanh

(defun tanh-fn (x)
  (etypecase x
    (single-float (%tanh-fn x))
    (number (%tanh-fn (coerce x 'single-float)))
    (array (smap-matrix-%tanh-fn x))))

(defun tanh-fn-deriv (x)
  (etypecase x
    (single-float (%tanh-fn-deriv x))
    (number (%tanh-fn-deriv (coerce x 'single-float)))
    (array (smap-matrix-%tanh-fn-deriv x))))

(defun %tanh-fn (x)
  (declare (optimize speed (safety 0))
	   (type single-float x))
  (tthe single-float (cond ((< x -10.0) -1.71591)
			   ((> x 10.0) 1.71591)
			   (t (%* 1.71591 (tanh (* 0.6666666 x)))))))

(defun %tanh-fn-deriv (fn)
  (declare (optimize speed (safety 0))
	   (type single-float fn))
  (tthe single-float (- 1.1539399 (* 0.38852075 fn fn)))) ;; 1.1439399 + 0.01

(define-single-float-mapper %tanh-fn)
(define-single-float-mapper %tanh-fn-deriv)

;; linear-fn

(declaim (inline linear-fn linear-fn-deriv))
(defun linear-fn (x) x)
(defun linear-fn-deriv (x)
  (etypecase x
    (number 1.0)
    (array (smap-matrix-%unit x))))

(defun %unit (x) (declare (ignore x)) (tthe single-float 1.0))
(define-single-float-mapper %unit)

;; other functions

(defun gauss-fn (val)
  (exp (- (* (square (- val 0.1)) 0.5))))

(defun gauss-kernel (v1 v2)
  (exp (- (* (square (e-norm (m- (copy v1) v2))) 0.5))))

(defun hpoly-kernel (v1 v2 &optional (deg 2))
  (expt (inner-prod v1 v2) deg))

(defun heaviside (val)
  (if (plusp val)
      1.0 0.0))

(define-single-float-mapper log)
(define-single-float-mapper exp)

(defun log-scale (val)
  (etypecase val
    (number (log val))
    (array (smap-matrix-log val))))

(defun log-unscale (val)
  (etypecase val
    (number (exp val))
    (array (smap-matrix-exp val))))

;; network parameters

(defun param (params name)
  (cdr (assoc name params :test #'eq)))

(defun (setf param) (val params name)
  (if (param params name)
      (setf (cdr (assoc name params :test #'eq)) val)
      (nconc params (list (cons name val))))
  params)

;; misc utils


(defun iota (num)
  (let ((iota (make-matrix num :element-type 'fixnum)))
    (dotimes (i num)
      (setf (aref iota i) i))
    iota))

(define-modify-macro extend-vector (&optional (delta 1))
  (lambda (x d) (adjust-array x (+ (length x) (or d 1)))))

(defun lastcar (list) (car (last list)))

(defun random-shuffle (sequence)
  (map-into sequence #'car
            (sort (map 'vector (lambda (x)
                                 (cons x (random 1.0)))
                       sequence)
                  #'< :key #'cdr)))

(defun random-shuffle-list (length)
  (random-shuffle
   (let ((lst (make-list length)))
     (dotimes (i length lst)
       (setf (elt lst i) i)))))

(defun random-elt (seq)
  (elt seq (random (length seq))))

(defun print-hash-table (hash-table)
  (loop for key being the hash-keys of hash-table
	using (hash-value value)
	do (print (list key value))))


(defun maphash-collect (hash-table)
  (loop for key being the hash-keys of hash-table
	using (hash-value value)
	collect (list key value)))

(defun last-not-nil (lst)
  (1- (length (remove nil lst))))

(defparameter *root-path*
  (let ((path (namestring (asdf:component-relative-pathname (asdf:find-system :annil)))))
    (if (search ".asd" path)
	(subseq path 0 (1+ (position #\/ path :from-end t)))
	path)))

(defun annil-relative (path)
  (concatenate 'string *root-path* path))

(defun deriv-fn-name (fn-name)
  (intern (concatenate 'string (string fn-name) "-DERIV") :annil))

(declaim (inline flatten-array-idx))
(defun flatten-array-idx (dim1 i j)
  (+ (* dim1 i) j))

(defun toss-coin ()
  (if (zerop (random 2)) t nil))

;; anaphoric

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
     (progn ,@body)))

(defun flatten (lis)
  (declare (list lis))
  (labels ((rec (lis acc)
             (cond ((null lis) acc)
                   ((atom lis) (cons lis acc))
                   (t (rec (car lis) (rec (cdr lis) acc))))))
    (rec lis nil)))

(defun mkstr (&rest args)
  (if args (format nil "~:@(~{~a~}~)" args)))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(deftype sf-seq ()
  '(simple-array single-float (*)))

(declaim (inline m1 e1))
(defun m1 (num) (make-matrix 1 :initial-element num))
(defun e1 (m1) (%fvref m1 0))
(defun append-sf (&rest seqs) (apply #'concatenate 'sf-seq seqs))

(defmacro collect-sf ((var count) &rest body)
  (let ((collect (gensym)))
    `(let ((,collect (make-matrix ,count)))
       (declare (optimize speed (safety 0)))
       (%dotimes (,var ,count ,collect)
	 (%setf (%fvref ,collect ,var)
		(progn ,@body))))))
