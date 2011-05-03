(in-package :annil)

(export '(info sigmoid-fn asigmoid-fn sigmoid-fn-deriv asigmoid-fn-deriv linear-fn linear-fn-deriv tanh-fn tanh-fn-deriv
	  param lastcar random-shuffle random-shuffle-list random-elt maphash-collect annil-relative deriv-fn-name))

(defmacro info (&rest body)
  `(progn (format *standard-output* " ;; ")
	  (format *standard-output* ,@body)))

;; activation functions

(defun sigmoid-fn (x)
  (cond ((< x -15.0) -0.5)
	((> x 15.0) +0.5)
	(t (- (/ 1.0 (+ 1.0 (exp (- x)))) 0.5))))

(defun asigmoid-fn (x)
  (cond ((< x -15.0) 0.0)
	((> x 15.0) 1.0)
	(t (/ 1.0 (+ 1.0 (exp (- x)))))))

(defun sigmoid-fn-deriv (fn)
;  (- 0.25 (* fn fn)))
  (* fn (- 1.0 fn)))

(defun asigmoid-fn-deriv (fn)
  (* fn (- 1.0 fn)))

(defun linear-fn (x) x)

(defun linear-fn-deriv (fn)
  (declare (ignore fn))
  1.0)
  
(declaim (ftype (function (single-float) single-float) tanh-fn tanh-fn-deriv))
(defun tanh-fn (x)
  (declare (optimize speed (safety 0))
	   (type single-float x))
  (cond ((< x -10.0) -1.0)
	((> x 10.0) 1.0)
	(t (* 1.71591 (the single-float (tanh (* 0.6666666 x)))))))

(defun tanh-fn-deriv (fn)
  (declare (optimize speed (safety 0))
	   (type single-float fn))
  (- 1.1439399 (* 0.38852075 fn fn)))

(defun gauss-fn (val)
  (exp (- (* (square (- val 0.1)) 0.5))))

(defun gauss-kernel (v1 v2)
  (exp (- (* (square (e-norm (m- (copy v1) v2))) 0.5))))

(defun hpoly-kernel (v1 v2 &optional (deg 2))
  (expt (inner-prod v1 v2) deg))

(defun heaviside (val)
  (if (plusp val)
      1.0 0.0))

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

(define-modify-macro extend-vector ()
  (lambda (x) (adjust-array x (1+ (length x)))))

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
