;;;; macros

;;; three main mechanisms to write good macro
;;; 1. backquote
;;; 2. comma
;;; 3. gensym

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro my-when (condition &rest body)
  `(if ,condition (progn ,@body)))

(defmacro my-unless (condition &rest body)
  `(if (not ,condition) (progn ,@body)))

;; backquote is nice, you can write the code without
;; quote everything.
(defmacro do-primes-1 ((var start end) &body body)
  `(do ((,var (next-prime ,start)
              (next-prime (1+ ,var))))
       ((> ,var ,end))
       ,@body))

;;; use gensym to avoid leaky abstraction
;; 1. use gensym to generate unique symbol.
;; 2. make sure only evaluate each subform once.
;; 3. make evaluation order the same as the parameter order.

(defmacro do-primes-2 ((var start end) &body body)
  (let ((end-value (gensym)))
    `(do ((,var (next-prime ,start)
                (next-prime (1+ ,var)))
          (,end-value ,end))
         ((> ,var ,end-value)) ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macro writing macro
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym))) ,@body))

(defmacro do-primes-with-gensyms ((var start end) &body body)
  (with-gensyms (end-value)
    `(do ((,var (next-prime ,start)
                (next-prime (1+ ,var)))
          (,end-value ,end))
         ((> ,var ,end-value))
         ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; common macro helps you to evaluate a symbol only once.
;; multiple layers because once-only is a macro itself so
;; it needs to be hygenic for itself too.
(defmacro once-only ((&rest names) &body body)
  ;; generate unique symbols for each name
  (let ((gensyms (loop for n in names collect (gensym))))
    ;;
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms
                       for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names
                         for g in gensyms collect `(,n ,g)))
             ,@body)))))

;; with once only things get much simler.
(defmacro do-primes ((var start end) &body body)
  (once-only (start end)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
     ((> ,var ,end))
     ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; oneliner macro
;;; there are a lot of restructions for mutation in common lisp.
;;; like if you want to make a form setable you need to have the
;;; corresponding place define with (DEFSETF).
;;; and because it's macro, a value (elt x x) returned from a function
;;; won't work.
;;; But sometimes you need to refer to one value again and again.
;;; like if I'm working with (elt x 1), I will need to address it multiple
;;; time in a body.

(defun foo ()
  (let ((xs '(1 2 3 4 5 6)))
    (macrolet ((3rd () `(elt xs 3)))
      (setf (3rd) 10)
      xs)))

(foo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; topevel def macros
;; use macros to generate functions

(defmacro gen-foo (v)
  (let ((fname (concatenate 'string
                            "foo-"
                            (symbol-name v)))
        (format t "~a" fname)
        `(defun ,fname () ,v))))
