(defpackage :annil-system
    (:use :common-lisp :asdf))

(in-package :annil-system)

(defsystem annil
  :name "annil"
  :description "Artificial Neural Networks in Lisp"
  :version "0.9"
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
	     (:file "network")
	     (:file "initialize")))
   (:module opt
	    :serial t
	    :components
	    ((:file "sse")
	     (:file "rprop")
	     (:file "quickprop")))
   (:module archs
	    :serial t
	    :components
	    ((:module cascor
		      :serial t
		      :components
		      ((:file "cascor")
		       (:file "cascor-train")))
	     (:module pca
		      :components
		      ((:file "autoscale")
		       (:file "svd")
		       (:file "nipals")))))
   (:module tasks
	    :components
	    ((:file "codec")
	     (:file "classify")))
   (:module tests
	    :components
	    ((:file "two-spirals")
	     (:file "symbols")
	     (:file "xor")
	     (:module elena
		      :components
		      ((:file "concentric")))))))
