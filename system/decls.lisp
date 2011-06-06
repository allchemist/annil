(in-package :annil)

(defmacro %incf (place &optional (delta 1.0))
  `(the single-float (incf (the single-float ,place)
			   (the single-float ,delta))))

(defmacro %decf (place &optional (delta 1.0))
  `(the single-float (decf (the single-float ,place)
			   (the single-float ,delta))))

(defmacro %setf (place &optional (delta 1.0))
  `(the single-float (setf (the single-float ,place)
			   (the single-float ,delta))))

(defmacro %* (&rest args)
  `(the single-float
     (* ,@(mapcar #'(lambda (x) `(the single-float ,x)) args))))

(defmacro %+ (&rest args)
  `(the single-float
     (+ ,@(mapcar #'(lambda (x) `(the single-float ,x)) args))))

(defmacro %- (&rest args)
  `(the single-float
     (- ,@(mapcar #'(lambda (x) `(the single-float ,x)) args))))

(defmacro %/ (&rest args)
  `(the single-float
     (/ ,@(mapcar #'(lambda (x) `(the single-float ,x)) args))))

(defmacro %square (num)
  `(the single-float (square (the single-float ,num))))

(defmacro %fvref (a i)
  `(the single-float (aref (the (simple-array single-float (*)) ,a) (the fixnum ,i))))

(defmacro %ivref (a i)
  `(the fixnum (aref (the (simple-array fixnum (*)) ,a) (the fixnum ,i))))

(defmacro %svref (a i)
  `(svref (the simple-vector ,a) (the fixnum ,i)))

(defmacro %ssvref (a i)
  `(the simple-vector (svref (the simple-vector ,a) (the fixnum ,i))))

(defmacro %dotimes (form1 &body body)
  `(dotimes ,form1 (declare (fixnum ,(car form1))) . ,body))
