(in-package :annil)

(defclass network ()
  ((doc :accessor doc)))

(defgeneric eval-network (network pattern))

(defgeneric print-network (network &optional dest))

(defclass tool ()
  ((doc :accessor doc)))
