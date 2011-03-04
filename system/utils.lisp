(in-package :annil)

(defun info (&rest body)
  (format *query-io* " ;; ")
  (eval `(format *query-io* ,@body)))

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
  
(defun tanh-fn (x)
  (cond ((< x -10.0) -1.0)
	((> x 10.0) 1.0)
	(t (* 1.7159 (tanh (* 2/3 x))))))

(defun tanh-fn-deriv (fn)
  (* 0.38852304 (- 1.7159 fn) (+ 1.7159 fn)))

;; network parameters

(defun param (params name)
  (cdr (assoc name params :test #'eq)))

(defun (setf param) (val params name)
  (if (param params name)
      (setf (cdr (assoc name params :test #'eq)) val)
      (nconc params (list (cons name val))))
  params)

;; misc utils

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
