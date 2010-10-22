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
    (nreverse patterns)))

(defun visual-classify-2spiral (network patterns range)
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
;	   (plot "'-'" with points pt 4 lc rgb "'green'" ", " "'-'" with points pt 4 lc rgb "'red'" ", " "'-'" with points pt 7 lc rgb "'blue'" ", " "'-'" with points pt 7 lc rgb "'black'")))
	   (plot "'-' pt 4, '-' pt 4 ps 0.5, '-' pt 7, '-' pt 7 ps 0.5")))
    (dolist (pat (list c1+ c1- c2+ c2-))
      (dotimes (i (num-patterns pat))
	(let ((p (get-pattern pat i)))
	  (gplt:gplt-exec `(,(elt (first p) 0) ,(elt (first p) 1)))))
      (gplt:gplt-exec '(e)))

    (gplt:gplt-display)))

(defun visual-test-2spirals (network num out-range classify-range)
  (let (class1 class2 unclassified)
    (dotimes (i num)
      (let* ((in (make-matrix 2 :initial-contents `(,(random-value 6.5) ,(random-value 6.5))))
	     (out (eval-network network in)))
	(cond ((< (elt out 0) (+ (first out-range) classify-range))
	       (push (list in out) class1))
	      ((> (elt out 0) (- (second out-range) classify-range))
	       (push (list in out) class2))
	      (t (push (list in out) unclassified)))))
    (gplt:gplt-restart)
    (map nil #'gplt:gplt-exec
	 `((unset key)
	   (unset color)
	   (plot "'-' pt 7 ps 0.5, '-' pt 7 ps 0.5, '-' pt 7 ps 0.5")))
    (dolist (p class1)
      (gplt:gplt-exec `(,(elt (first p) 0) ,(elt (first p) 1))))
    (gplt:gplt-exec '(e))
    (dolist (p class2)
      (gplt:gplt-exec `(,(elt (first p) 0) ,(elt (first p) 1))))
    (gplt:gplt-exec '(e))
    (dolist (p unclassified)
      (gplt:gplt-exec `(,(elt (first p) 0) ,(elt (first p) 1))))
    (gplt:gplt-exec '(e))

    (gplt:gplt-display)))
