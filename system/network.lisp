(in-package :annil)

(export '(network eval-network compute-network-err))

(defclass network ()
  ((doc :accessor doc)))

(defgeneric eval-network (network input))
(defgeneric print-network (network &optional dest))

(defun compute-network-err (network patterns &optional classify-range)
  (let ((err-sum 0.0) (err-bits 0) (pos-err-bits 0))
    (assert (patterns-with-goals patterns) nil "You need goals to computer error")
    (do-patterns-safe (patterns p)
      (let ((err (m- (eval-network network (first p)) (second p))))
	(incf err-sum (square (e-norm err)))
	(when classify-range
	  (dotimes (i (dim0 err))
	    (when (> (abs (aref err i)) classify-range)
	      (incf err-bits)
	      (when (plusp (aref (second p) 0)) (incf pos-err-bits)))))))
    (values (/ err-sum (num-patterns patterns))
	    (when classify-range (- err-bits pos-err-bits))
	    (when classify-range pos-err-bits))))
