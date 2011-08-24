(in-package :annil)

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
    (gplt:gprestart)
    (map nil #'gplt:gpexec
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
	  (gplt:gpexec `(,(elt (first p) 0) ,(elt (first p) 1)))))
      (gplt:gpexec '(e)))

    (gplt:gpdisplay)))


(defun demo-2spirals-weak (&optional (patterns (genpat-2spirals)))
  (info "Demonstration: solving two spirals problem with cascade2 network~%~%") (sleep 1)
  (make-cascade-classifier
   patterns nil
   '((:act-fn . :tanh) (:eps . 0.2) (:mu . 2.0) (:epochs . 150) (:thr . 1.e-4) (:recompute . 20)
     (:verbosity . 2) (:max-err-bits . 0.) (:misclassify-limit . 0.4))
   '((:eps . 1.0) (:mu . 2.0) (:epochs . 200) (:thr . 1.e-5) (:recompute . 10) (:verbosity . 0)
     (:candidates . 10) (:hidden-num . 20) (:epochs-handicap . 0)) ;nil))
   nil (lambda (n) (visual-test-2d n 20000 6.5 '(-1.0 1.0) 0.4))))

(defun demo-2spirals-boost (&optional (patterns (genpat-2spirals)))
  (info "Demonstration: solving two spirals problem with adaboost over cascade2 networks~%~%") (sleep 1)
  (make-adaboost-network
   patterns
   #'(lambda (pats)
       (make-cascade-classifier
	pats nil
	'((:act-fn . :tanh) (:eps . 0.2) (:mu . 2.0) (:epochs . 150) (:thr . 1.e-4) (:recompute . 20)
	  (:verbosity . 2) (:max-err-bits . 0.35) (:misclassify-limit . 0.4))
	'((:eps . 1.0) (:mu . 2.0) (:epochs . 200) (:thr . 1.e-5) (:recompute . 10) (:verbosity . 0)
	  (:candidates . 10) (:hidden-num . 20) (:epochs-handicap . 0)) ;nil))
	nil nil))
   '((:verbosity . t) (:max-weak . 3))))
