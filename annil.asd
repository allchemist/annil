(defpackage :annil-system
    (:use :common-lisp :asdf))

(in-package :annil-system)

(defsystem annil
  :name "annil"
  :description "Artificial Neural Networks in Lisp"
  :version "0.8"
  :author "Khokhlov Ivan"
  :licence "BSD"
  :depends-on (sb-math gplt cl-jpeg)
  :serial t
  :components
  ((:module system
	    :serial t
	    :components
	    ((:file "package")
	     (:file "utils")
	     (:file "patterns")
	     (:file "network")))
   (:module opt
	    :serial t
	    :components
	    ((:file "univariate")
	     (:file "quickprop")))
   (:module archs
	    :serial t
	    :components
	    ((:module slp
		      :components
		      ((:file "slp")
		       (:file "slp-train")))
	     (:module cascor
		      :serial t
		      :components
		      ((:file "cascor")
		       (:file "cascor-train")
		       ;(:file "fcascor")
		       ))
	     (:module hebbian
		      :components
		      ((:file "gha")))))
   (:module tasks
	    :components
	    ((:file "preproc")
	     (:file "classification")
	     (:file "coding" :depends-on ("preproc"))))
   (:module tests
	    :components
	    ((:file "two-spirals")
	     (:file "symbols")
	     (:file "xor")))))
