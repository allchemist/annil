(in-package :annil)

(defmacro tthe (value-type form)
  `(sb-ext:truly-the ,value-type ,form))

(defmacro %incf (place &optional (delta 1.0))
  `(tthe single-float (incf (the single-float ,place)
			    (tthe single-float ,delta))))

(defmacro %decf (place &optional (delta 1.0))
  `(tthe single-float (decf (the single-float ,place)
			    (tthe single-float ,delta))))

(defmacro %setf (place &optional (delta 1.0))
  `(tthe single-float (setf (the single-float ,place)
			    (tthe single-float ,delta))))

(defmacro %* (&rest args)
  `(tthe single-float
	 (* ,@(mapcar #'(lambda (x) `(tthe single-float ,x)) args))))

(defmacro %+ (&rest args)
  `(tthe single-float
	 (+ ,@(mapcar #'(lambda (x) `(tthe single-float ,x)) args))))

(defmacro %- (&rest args)
  `(tthe single-float
	 (- ,@(mapcar #'(lambda (x) `(tthe single-float ,x)) args))))

(defmacro %/ (&rest args)
  `(tthe single-float
	 (/ ,@(mapcar #'(lambda (x) `(tthe single-float ,x)) args))))

(defmacro %square (num)
  `(tthe single-float (square (tthe single-float ,num))))

(defmacro %fvref (a i)
  `(the single-float (aref (the (simple-array single-float (*)) ,a) (tthe fixnum ,i))))

(defmacro %ivref (a i)
  `(the fixnum (aref (the (simple-array fixnum (*)) ,a) (tthe fixnum ,i))))

(defmacro %svref (a i)
  `(svref (the simple-vector ,a) (tthe fixnum ,i)))

(defmacro %ssvref (a i)
  `(the simple-vector (svref (the simple-vector ,a) (tthe fixnum ,i))))

(defmacro %dotimes (form1 &body body)
  `(dotimes ,form1 (declare (fixnum ,(car form1))) . ,body))
