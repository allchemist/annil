(in-package :annil)

(defun goldsect (func a b eps iter)
  (let* ((g1 (/ (- (sqrt 5) 1) 2))
         (g2 (- 1 g1))
         (x1 (+ a (* (abs (- b a)) g2)))
         (x2 (+ a (* (abs (- b a)) g1)))
         (f1 (funcall func x1))
         (f2 (funcall func x2)))
    (dotimes (i iter)
      (if (<= f1 f2)
          (progn
            (setq b x2)
            (setq x2 x1)
            (setq f2 f1)
            (setq x1 (+ a (*  (- b a) g2)))
            (setq f1 (funcall func x1)))
          (progn
            (setq a x1)
            (setq x1 x2)
            (setq f1 f2)
            (setq x2 (+ a (* (- b a) g1)))
            (setq f2 (funcall func x2))))
      (if (< (abs (- b a)) eps) (return (/ (+ a b) 2))))
    (/ (+ a b) 2)))
