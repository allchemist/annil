(in-package :annil)

(export '(slp make-slp-network train-slp-network))

(defclass slp ()
  ((weights :initarg :weights :accessor slp-weights)
   (act-fn  :initarg :act-fn  :accessor net-act-fn)
   (out     :initarg :out     :accessor slp-out)))

(defun make-slp-network (input-size output-size act-fn)
  (make-instance 'slp
		 :weights (seed-weights (make-matrix `(,output-size ,input-size)))
		 :act-fn act-fn
		 :out (make-matrix output-size)))

(defun eval-slp-network (slp input)
  (activation (gemv (slp-weights slp) input :dest (slp-out slp))
	      (net-act-fn slp)))

(defmethod eval-network ((network slp) input)
  (eval-slp-network network input))

(defun train-slp-network (slp train-patterns test-patterns params)
  (optimize-sse (slp-weights slp) train-patterns test-patterns (net-act-fn slp) params)
  slp)
