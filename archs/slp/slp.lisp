(in-package :annil)

;; data structures

(defclass slp (network)
  ((weights :initarg :weights :accessor slp-weights)
   (act-fn  :initarg :act-fn  :accessor net-act-fn)))

;; initializing

(defun make-slp (input-size output-size act-fn)
  (make-instance 'slp
		 :weights (make-matrix `(,output-size ,input-size))
		 :act-fn  act-fn))

(defun make-random-slp (input-size output-size act-fn &optional (rng 'simple-rng))
  (let ((slp (make-slp input-size output-size act-fn)))
    (map-matrix (slp-weights slp) rng)
    slp))

;; evaluating

(defun eval-slp (slp input &key dest)
  (map-matrix (gemv (slp-weights slp) input :dest dest) (net-act-fn slp)))

(defmethod eval-network ((network slp) input)
  (eval-slp network input))
