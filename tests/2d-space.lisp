(in-package :annil)

;; deprecated!

(defun display-2d-patterns (patterns &optional ranges out)
  (let ((outputs (remove-duplicates (map 'list #'(lambda (p) (aref (second p) 0))  patterns))))
    (gplt:gprestart)
    (map nil #'gplt:gpexec
	 `((unset key)
	   (unset color)
	   ,@(when ranges `((set xrange (range ,@ranges))
			    (set yrange (range ,@ranges))))
	   ,@(if out
		 `((set term png)
		   (set out ,(write-to-string out)))
		 `((set term x11)))
	   (plot ,(let ((str (apply #'concatenate 'string
				    (loop for o in outputs collect "'-' pt 7 ps 2, "))))
		    (subseq str 0 (- (length str) 2))))))
    (dolist (o outputs)
      (let ((pats (remove-if #'(lambda (p) (/= (aref (second p) 0) o)) patterns)))
	(dotimes (i (length pats))
	  (let ((p (svref pats i)))
	    (gplt:gpexec `(,(elt (first p) 0) ,(elt (first p) 1))))))
      (gplt:gpexec '(e)))
    (gplt:gpdisplay)))

(defun visual-test-2d (network num in-range out-range classify-range &optional out)
  (let (class1 class2 unclassified)
    (dotimes (i num)
      (let* ((in (make-matrix 2 :initial-contents
			      `(,(random-value in-range)
				,(random-value in-range))))
	     (out (eval-network network in)))
	(cond ((< (elt out 0) (+ (first out-range) classify-range))
	       (push (list in out) class1))
	      ((> (elt out 0) (- (second out-range) classify-range))
	       (push (list in out) class2))
	      (t (push (list in out) unclassified)))))
    (gplt:gprestart)
    (map nil #'gplt:gpexec
	 `((unset key)
	   (unset color)
	   ,@(if out
		 `((set term png)
		   (set out ,(write-to-string out)))
		 `((set term x11)))
	   (plot "'-' pt 7, '-' pt 7")));, '-' pt 7")))
    (dolist (p class1)
      (gplt:gpexec `(,(elt (first p) 0) ,(elt (first p) 1))))
    (gplt:gpexec '(e))
    (dolist (p class2)
      (gplt:gpexec `(,(elt (first p) 0) ,(elt (first p) 1))))
    (gplt:gpexec '(e))
;    (dolist (p unclassified)
;      (gplt:gpexec `(,(elt (first p) 0) ,(elt (first p) 1))))
;    (gplt:gpexec '(e))

    (gplt:gpdisplay)))
