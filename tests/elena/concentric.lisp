(in-package :annil)

(defun genpat-elena-concentric ()
  (let (patterns)
    (with-open-file (s (concatenate 'string *root-path* "tests/elena/concentric.dat"))
      (loop for line = (read-line s nil nil) while line
	    do (let ((str (make-string-input-stream line)))
		 (push (list (make-matrix 2 :initial-contents `(,(read str) ,(read str)))
			     (make-matrix 1 :initial-contents `(,(coerce (read str) 'single-float))))
		       patterns))))
    (coerce (nreverse patterns) 'simple-vector)))
