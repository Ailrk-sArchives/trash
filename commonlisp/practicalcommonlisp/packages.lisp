;; common lisp package system is based on symbols
;; a name with : in the middle is interpreted as a qualified name,
;; and the common lisp reader will separate the name and use the first
;; part as the pacakge name, the second part as the function name.

;; symbols are real data structures, and it's also the text used by the
;; s-expression. You can use symbols to construct a piece of textual form
;; of common lisp code and evaluate it as it's a real code.

;; A lisp form is a piece of s-expression to be evaluated.
;; when you evaluate a symbol as lisp form it means you are trying to
;; evaluate whatever form associated with that symbol.

;; to assoicate a symbol with other lisp forms, say a macro, you need to
;; first generate a string as the name of the symbol, then genrate the symbol
;; from the string.

;; to intern a symbol
;; intern will take the string and check if the name already exists in the
;; current pacakge. If it does, it will create and add a new symbol into the package.
(intern "bar")

(eql ':foo :foo) ;; => t
(symbol-name :foo)  ;; => "FOO"

;; You can also have unintended symbol. which each time you read the symbol
;; it will be a new symbol (like gensym)
(eql '#:foo '#:foo) ;; => nil
(gensym)

(defpackage "SAMPLE-PACKAGE"
  (:use "COMMON-LISP")
  (:export :hello-world
           :good-day))

;; bring you to sample-package namespace
(in-package :sample-package)

(intern "hello-world")
(defun hello-world ()
  (format t "hello world ~%"))

(defun good-day ()
  (format t "good day mate ~%"))

