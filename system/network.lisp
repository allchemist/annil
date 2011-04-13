(in-package :annil)

(export '(network doc eval-network print-network tool))

(defclass network ()
  ((doc :accessor doc)))

(defgeneric eval-network (network pattern))

(defgeneric print-network (network &optional dest))

(defclass tool ()
  ((doc :accessor doc)))
