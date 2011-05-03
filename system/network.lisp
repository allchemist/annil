(in-package :annil)

(export '(network doc eval-network print-network tool compute-network-err))

(defclass network ()
  ((doc :accessor doc)))

(defgeneric eval-network (network pattern))

(defgeneric print-network (network &optional dest))

(defclass tool ()
  ((doc :accessor doc)))

(defun compute-network-err (network patterns classify-range)
  (let ((err-sum 0.0) (err-bits 0))
    (do-patterns-safe (patterns p)
      (let ((err (m- (eval-network network (first p)) (second p))))
	(incf err-sum (square (e-norm err)))
	(dotimes (i (dim0 err))
	  (when (> (abs (aref err i)) classify-range)
	    (incf err-bits)))))
    (values (/ err-sum (num-patterns patterns)) err-bits)))
