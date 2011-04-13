(in-package :annil)

(defclass slp (network)
  ((weights :initarg :weights :accessor slp-weights)
   (act-fn  :initarg :act-fn  :accessor net-act-fn)))

(defun make-slp (input-size output-size act-fn)
  (make-instance 'slp
		 :weights (make-matrix `(,output-size ,input-size))
		 :act-fn  act-fn))

(defun make-random-slp (input-size output-size act-fn &optional (rng 'simple-rng))
  (let ((slp (make-slp input-size output-size act-fn)))
    (map-matrix (slp-weights slp) rng)
    slp))

(defun eval-layer (input weights act-fn &key dest)
  (map-matrix (gemv weights input :dest dest) act-fn))

(defun eval-slp (slp input &key dest)
  (eval-layer input (slp-weights slp) (net-act-fn slp) :dest dest))

(defmethod eval-network ((network slp) input)
  (eval-slp network input))
