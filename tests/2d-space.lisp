(in-package :annil)

(defun display-2d-patterns (patterns &optional ranges out)
  (let ((outputs (remove-duplicates (mapcar #'(lambda (p) (aref (second p) 0))  patterns))))
    (gplt:gplt-restart)
    (map nil #'gplt:gplt-exec
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
	    (gplt:gplt-exec `(,(elt (first p) 0) ,(elt (first p) 1))))))
      (gplt:gplt-exec '(e)))
    (gplt:gplt-display)))
