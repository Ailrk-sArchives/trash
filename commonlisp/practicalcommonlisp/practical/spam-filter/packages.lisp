;; bayesian spam filter.
(in-package :cl-user)

(defpackage :spam-filter
  (:nickname :spam-filter)
  (:use :common-lisp
        :com.ailrk.pathnames
        :com.ailr.utilities)
  (:export
   :make-feature-database
   :clear-database
   :intern-feature
   :train
   :untrain
   :classify
   :all-features
   :freature-spamminess
   :id
   :hams
   :spams))
