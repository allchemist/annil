(in-package :annil)

(gplt:gplt-start)

(defun 2spiral-radius (i) (/ (* 6.5 (- 104 i)) 104))
(defun 2spiral-angle (i) (* i (/ (coerce pi 'single-float) 16)))

(defun genpat-2spirals (&optional (output-ranges '(-1.0 1.0)))
  (let ((patterns nil))
    (dotimes (i 97)
      (let ((r (2spiral-radius i))
	    (a (2spiral-angle i)))
	(let ((x (* r (sin a)))
	      (y (* r (cos a))))
	  (push (list (make-matrix 2 :initial-contents `(,x ,y))
		      (make-matrix 1 :initial-element (first output-ranges)))
		patterns)
	  (push (list (make-matrix 2 :initial-contents `(,(- x) ,(- y)))
		      (make-matrix 1 :initial-element (second output-ranges)))
		patterns))))
    (coerce (nreverse patterns) 'simple-vector)))

(defun visual-classify-2spirals (network patterns range &optional out)
  (let (c1+ c1- c2+ c2-)
    (dotimes (i (length patterns))
      (let ((p (get-pattern patterns i)))
	(if (oddp i)
	    (if (< (abs (ammax (m- (eval-network network (first p)) (second p)))) range)
		(push p c1+) (push p c1-))
	    (if (< (abs (ammax (m- (eval-network network (first p)) (second p)))) range)
		(push p c2+) (push p c2-)))))
    (gplt:gplt-restart)
    (map nil #'gplt:gplt-exec
	 `((unset key)
	   (unset color)
	   ,@(if out
		 `((set term png)
		   (set out ,(write-to-string out)))
		 `((set term x11)))
	   (plot "'-' pt 4, '-' pt 4 ps 0.5, '-' pt 7, '-' pt 7 ps 0.5")))
    (dolist (pat (mapcar #'(lambda (p) (coerce p 'vector)) (list c1+ c1- c2+ c2-)))
      (dotimes (i (num-patterns pat))
	(let ((p (get-pattern pat i)))
	  (gplt:gplt-exec `(,(elt (first p) 0) ,(elt (first p) 1)))))
      (gplt:gplt-exec '(e)))

    (gplt:gplt-display)))

(defun demo-2spirals (&optional (patterns (genpat-2spirals)))
  (info "Patterns number: ~A~%" (num-patterns patterns))
  (make-instance
   'simple-classifier
   :net (cascade-train (make-cascade-network 2 1 'tanh-fn nil) patterns nil 10
		       '((:eps . 0.35) (:mu . 2.0) (:epochs . 100) (:thr . 1.e-6) (:recompute . 30) (:verbosity . 2)
			 (:classify-range . 0.8) (:test-set-mix-ratio . 3))
		       '((:eps . 0.35) (:mu . 2.0) (:epochs . 100) (:thr . 1.e-6) (:recompute . 30) (:verbosity . 0)
			 (:candidates . 5) (:test-set-mix-ratio . 2) (:epochs-handicap . 40))
		       (lambda (n)
			 (visual-test-2d n 15000 6.5 '(-1.0 1.0) 0.9)))
   :range 0.8))

