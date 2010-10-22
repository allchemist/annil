(in-package :annil)

(defparameter *symbols-list*
  '("A" "B" "C" "D" "E" "F" "G" "H" "I" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "Y" "Z"))
(defparameter *symbols-path* (annil-relative "tests/symbols-data/"))

(require :cl-jpeg)

(defun prepare-inputs (path)
  (let* ((decode (jpeg:decode-image path))
	 (matrix (make-matrix (length decode))))
    (dotimes (i (length matrix))
      (setf (aref matrix i) (float (aref decode i))))
    (normalize matrix)))

(defun prepare-outputs (length pos ranges)
  (let ((output (m-c (make-matrix length) (- (first ranges)))))
    (setf (elt output pos) (second ranges))
    output))

(defun genpat-symbols (num &optional (ranges '(-1.0 1.0)) (path *symbols-path*))
  (let ((patterns nil)
	(symbols (mapcar #'(lambda (x) (cons x num)) *symbols-list*)))
    (dolist (s symbols)
      (dotimes (i (cdr s))
	(push
	 (list (prepare-inputs (concatenate 'string path (car s) (write-to-string i) ".jpeg"))
	       (prepare-outputs (length symbols) (position s symbols) ranges))
	 patterns)
	(info "Loaded ~A~A~A.png~%" path (car s) i)))
    (info "Generated ~A samples.~%" (length patterns))
    (nreverse patterns)))

;; visual

(defun decode-output (output)
  (elt *symbols-list* (imax output)))

(defun visual-classify-symbols (network patterns)
  (let ((net (typecase network (network network) (classifier (classifier-net network)))))
    (do-patterns (patterns p)
      (let ((waited (decode-output (second p)))
	    (got (decode-output (eval-network net (first p)))))
	(info "Waited: ~A, got ~A~%" waited got)))))
