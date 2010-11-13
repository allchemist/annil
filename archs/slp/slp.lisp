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

(defun eval-layer (input weights act-fn &key dest)
  (map-matrix (gemv weights input :dest dest) act-fn))

(defun eval-slp (slp input &key dest)
  (eval-layer input (slp-weights slp) (net-act-fn slp) :dest dest))

(defmethod eval-network ((network slp) input)
  (eval-slp network input))

;; error

(defun sse-layer-error (pattern weights act-fn &key dest)
  (square (e-norm (m- (eval-layer (first pattern) weights act-fn :dest dest) (second pattern)))))

(defun sse-patterns-layer-error (patterns weights act-fn)
  (let ((err 0)
	(dest (make-matrix (patterns-output-dim patterns))))
    (do-patterns (patterns p)
      (incf err (sse-layer-error p weights act-fn :dest dest)))
    (/ err (num-patterns patterns))))

(defun slp-pattern-error (slp pattern)
  (sse-layer-error pattern (slp-weights slp) (net-act-fn slp)))

(defun slp-patterns-error (slp patterns)
  (sse-patterns-layer-error patterns (slp-weights slp) (net-act-fn slp)))
