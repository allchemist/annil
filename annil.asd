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
	     (:file "decls")
	     (:file "utils")
	     (:file "patterns")
	     (:file "network")
	     (:file "initialize")))
   (:module opt
	    :serial t
	    :components
	    ((:file "regression")
	     (:file "sse")
	     ;; (:file "rprop")
	     (:file "quickprop")))
   (:module preproc
	    :components
	    ((:file "centering")
	     (:file "autoscale")
	     (:file "outputs")
	     (:file "pca-nipals")
	     (:file "pca-svd")))
   (:module tseries
	    :serial t
	    :components
	    ((:file "stats")
	     (:file "tseries")
	     (:file "tseries-ops")))
   (:module arch
	    :serial t
	    :components
	    ((:module slp
		      :components
		      ((:file "slp")))
	     (:module cascade
		      :serial t
		      :components
		      ((:file "cascade")
		       (:file "cascade-train-c2")
		       (:file "cascade-train")
		       (:file "recurrent-cascade")
		       (:file "recurrent-cascade-train")))
	     (:module boost
		      :serial t
		      :components
		      ((:file "adaboost")))))
   (:module tasks
	    :components
	    ((:module codec
		      :serial t
		      :components
		      ((:file "simple")
		       (:file "pca")))
	     (:module classification
		      :serial t
		      :components
		      ((:file "classifier")))
	     (:module predict
		      :serial t
		      :components
		      ((:file "tseries-patterns")
		       (:file "predict")))))
   (:module tests
	    :components
	    ((:file "2d-space")
	     (:file "xor")
	     (:file "two-spirals")))
   ))
