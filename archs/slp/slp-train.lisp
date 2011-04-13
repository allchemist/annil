(in-package :annil)

(defun slp-train (slp train-patterns test-patterns params)
  (optimize-sse (slp-weights slp) train-patterns test-patterns (net-act-fn slp) params)
  slp)
