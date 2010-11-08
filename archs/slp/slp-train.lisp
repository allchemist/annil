(in-package :annil)

(defun slp-train-quickprop (slp train-patterns test-patterns params)
  (quickprop-sse (slp-weights slp) train-patterns test-patterns (net-act-fn slp) params)
  slp)

(defun slp-train-quickprop-mix (slp patterns train-part params)
  (quickprop-mix-sse (slp-weights slp) patterns train-part (net-act-fn slp) params)
  slp)

(defun slp-train (slp train-patterns test-patterns params &optional (method 'quickprop))
  (case method
    (quickprop (slp-train-quickprop slp train-patterns test-patterns params))
    (quickprop-mix (slp-train-quickprop-mix slp train-patterns test-patterns params))
    (t (error "Unknown training method: ~A~%" method))))
