(defpackage :annil
    (:use :common-lisp :sb-math)
  (:export
   ;; utils
   :info
   :logistic-fn
   :logistic-fn-deriv
   :linear-fn
   :linear-fn-deriv
   :tanh-fn
   :tanh-fn-deriv
   :get-param
   :set-param
   :lastcar
   :random-shuffle-list
   :print-hash-table
   :maphash-collect
   :symm-rng
   ;; patterns
   :matrix-patterns-p
   :list-patterns-p
   :get-pattern
   :num-patterns
   :patterns-input-dim
   :patterns-output-dim
   :patterns-to-list
   :patterns-to-matrix
   :add-patterns
   :copy-patterns
   :random-shuffle-patterns
   :do-patterns
   :do-patterns-shuffle
   :split-patterns
   ;; perceptron
   :perceptron
   :make-perceptron
   :perceptron-weights
   :perceptron-nodes
   :perceptron-layers
   :perceptron-act-fn
   :perceptron-grads
   :randomly-fill-perceptron
   :make-random-perceptron
   :print-perceptron
   :eval-perceptron
   ;; backprop

   ))
