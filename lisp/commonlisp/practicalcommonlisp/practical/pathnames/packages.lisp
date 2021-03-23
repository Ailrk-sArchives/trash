(in-package :cl-user) ;; use cl namespace

(defpackage :pathnames
  (:use :common-lisp)   ;; only uses common lisp standard lib
  (:export
    :list-directory
    :file-exists-p
    :directory-pathname-p
    :file-pathname-p
    :pathname-as-directory
    :pathname-as-file
    :walk-directory
    :directory-p
    :file-p
    :parent-directory))
