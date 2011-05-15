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
	     (:file "visual")
	     (:file "initialize")))
   (:module opt
	    :serial t
	    :components
	    ((:file "regression")
	     (:file "sse")
	     ;; (:file "rprop")
	     (:file "quickprop")))
   (:module archs
	    :serial t
	    :components
	    ((:module slp
		      :serial t
		      :components
		      ((:file "slp")
		       (:file "slp-train")))
	     (:module cascade
		      :serial t
		      :components
		      ((:file "cascade")
		       (:file "cascade-prune")
		       (:file "cascade-train")
		       (:file "cascade-train-c2"))
		      )
	     (:module prune
		      :serial t
		      :components
		      ((:file "obd")
		       (:file "obs")))
	     (:module pca
		      :components
		      ((:file "autoscale")
		       (:file "centering")
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
