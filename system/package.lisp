(in-package :annil-system)

(macrolet
    ((without-package-variance-warnings (&body body)
       `(eval-when (:compile-toplevel :load-toplevel :execute)
	  (handler-bind (#+sbcl(sb-int:package-at-variance #'muffle-warning))
	    ,@body))))
  (without-package-variance-warnings
   (defpackage :annil
       (:use :common-lisp :sb-alien :sb-math)
     )))
