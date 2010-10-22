(in-package :annil)

(defun slp-train-quickprop (slp patterns iter params)
  (quickprop-sse (slp-weights slp) patterns (net-act-fn slp) iter params))

(defun slp-train (slp patterns iter params &optional (method 'quickprop))
  (case method
    (quickprop (slp-train-quickprop slp patterns iter params))
    (t (error "Unknown training method: ~A~%" method))))
