(in-package :annil)

(export '(variance))

(defun variance (seq &key (start 0) end mean)
  (declare (optimize speed (safety 0)))
  (unless end (setf end (array-total-size seq)))
  (unless mean (setf mean (mean seq :start start :end end)))
  (let ((var 0.0))
    (loop for i from start below end do
      (%incf var (%square (%- (%fvref seq i) mean))))
    (values (/ var (- end start 1)) mean)))
