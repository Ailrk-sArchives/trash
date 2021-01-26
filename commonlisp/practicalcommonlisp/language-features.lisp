;;;; Chapter 5 - 13, some language features

;;;; Rule of thunbs
;;; 1. lisp always evaluate all expressions
;;; 2. atoms are self-evaluating
;;; 3. symbols stores 2 types of values:
;;;    regular value and functional definition. (2-lisp)
;;; 4. given S-expression, if quoted, return unevaluated
;;;    otherwise get it's car and evaluate.

(defun plot (fn min max step)
  ; use funcall to avoid evaluating the symbol for function name.
  (loop for i from min to max by step do
        (loop repeat (funcall fn i) do (format t "*"))
        (format t "~%")))

(defun funcall-and-apply ()
  ; this work, lambda is self-evaluating
  (write (funcall (lambda (x) (+ x 1)) 1))
  (write (funcall #'evenp 2))

  ; apply is the same as funcall, but takes arguments as a list
  (write (apply (lambda (x) (+ x 1)) '(1)))
  (write (apply #'evenp '(2)))
  ; (funcall evenp 2) this doesn't work because evenp get evaluated.
  )


(defun hello-world ()
  (format t "Hello world"))

(defun verbose-sum (x y)
  "Sum two numbers, and let everybody knows it"
  (format t "Yo, adding ~d and ~d now ~%" x y)
  (+ x y))

(defun optional-foo (a b &optional c d (e 10) (f '(good bad) f-p))
  (list a b c d e f f-p))

;;; &rest and &key are safe to combine together.
;;; &optional is the most problematic when combine with others.
(defun mixed-foo (a b &rest res &key x y z)
  (list a b x y z (length res)))

(defun foo (&key a (b 20) (c 30 c-p)) (list a b c c-p))
(defmacro varadic (&rest rs) (write rs))

(defun grow- (a b) (if (= b 0) a (grow- b (mod a b))))

(defun foo-return (n)
  (dotimes (i 10)
    (dotimes (j 10) ; return-from a block
      (when (> (* i j) n) (return-from foo-return (list i j))))))

;;; macro gym
(defmacro backward (expr) (reverse expr)) ; macro
(defun hello-macro ()
  (backward ("hello, world" t format)))

;;; ('a) use tip to avoid evaluate
;;; `(a ,b) back tip to not evaluate by default, use , to evaluate
(defun backtip () `(and ,(list 1 2 3)))     ; normal
(defun backtip* () `(and ,@(list 1 2 3)))    ; splice
(defun backtip** () `(and ,@(list 1 2 3) 4 5))    ; splice
(backtip**)

;;; chapter 3 row database
;ll property list with keyword symbol
(defparameter *propertylist* (list :a 1 :b 2 :c 3))
(getf *propertylist* :a)

;; chapter 6 variables

(defvar *var* 1)

;; introduce variable bindings
(defun foo (x y) (let ((z 10))
    (+ x y z)))


;; closure
(defparameter *fn*
  (let ((count 0))
    (lambda () (setf count (1+ count)))))

(defun funcscall (fns)
  (car (last (mapcar (lambda (fn) (funcall fn)) fns))))

(defparameter *fn1*
  (funcscall
    (let ((count 0))
      (list
        (lambda () (incf count))
        (lambda () (incf count))
        (lambda () (decf count))
        (lambda () count)))))

;; dynamic variables (special variables)
(defvar *a-counter* 0
  "Count of wdgets made so far")

(defvar *unbound*) ; this one is unbound

; (defparameter *unbound1*)
; defparameter always assign the inital value to the name
; so you can't have unbound variable.

(defparameter *gap-tolerance* 0.001
  "Tolerance to be allowed in widget gaps.")

; reload will keep the previous *a-counter* value
(defun increment-widget-count () (incf *a-counter*))

; each reload rebind the variable.
(defun increment-widget-gap-tolerance ()
  (setf *gap-tolerance* (1+ *gap-tolerance*)))


(defvar *x* 8)
(defun dynamicfoo ()
  (format t "outside of let binding, *x* is now ~a~%" *x*)
  (let ((*x* 20))
    (setf *x* 99)
    (format t "*x* is ~a~%" *x*))
  (format t "outside of let binding, *x* is now ~a~%" *x*))

; just lexical binding, nothing much to say.
(defun lexical-shadowing (x)
  (format t "the real parameter ~a~%" x)
  (let ((x 10))
    (format t "one layer down x: ~a~%" x)
    (let ((x 99))
      (format t "inner most x: ~a~%" x))))

; note all global variables are dynamically binded.
; see this example:
(defparameter *par* 5) ; define some global variables
(defvar *var* 10)
; the closure should always be the global if it's lexical
; scoped
(defun dynamic-scope-test () (+ *par* *var*))
(defun call-dynamic-scope-test ()
  (dynamic-scope-test) ; use the global binding like normal
  (let ((*par* 10)     ; we can sneaky in some other values.
        (*var* 20))
    ; however here it uses the local here.
    (dynamic-scope-test)))


; of course you have constant
(defconstant +constant+ 1)

; explanation:
; the value y is binded to 20
; then f is called,
; the value of y binds to, 20, now bind to value 10.
; it's a copy so to speak.
(defun set-lexical-binding ()
  (let ((f (lambda (x)
             (setf x 10) ; set the local binding.
             (prin1 x)
             ))
        (y 20))
    (funcall f y)
    (print y)))          ; binding y doesn't change

(defun set-by-closure () ; if you want to change value, use closure.
  (let* ((y 10)
         (f (lambda ()
              (setf y 20)
              (print y))))
    (funcall f)
    (print y)))


; setf with side effect.
(defparameter *array* (make-array 10))
(defparameter *vector* #(1 2 3 4))

(defun inc-random-array-cell ()
  (incf (aref *array* (random (length *array*)))))

; ref to the same cell twice
(defun inc-random-array-cell-with-setf-1 ()
  (let* ((idx (random (length *array*))))
    (setf (aref *array* idx) (+ (aref *array* idx) 1))))

; use getf to get the reference.
(defun inc-random-array-cell-with-setf ()
  (let* ((idx (random (length *array*)))
         (cell (aref *array* idx)))
    (setf (getf cell) (+ (getf cell) 1))))

; swap
(defun try-rotate (x y)
  (print `(,x ,y))
  (rotatef x y)
  `(,x ,y))

(defun my-rotate (a b)
  (let ((tmp a))
    (setf a b b tmp)
    nil))

; shift value one to the right.
(defun try-shift (a b)
  (shiftf a b 10)
  (list a b))

;; lisp 2
;; common lisp is lisp 2, so it's variable name and function names
;; are in different namespaces.
;; when common lisp sees form (f a b c d), name a will be looked
;; up in function space and a b c d will be from variable space.
;; this is why you have let and flet.
;; use flet to introduce a local binding.

;; use let* and labels for the usual cases,
;; let and labels are used only for cases that you explicitly
;; don't want closure or don't want recursive binds.

(defun lisp2-let ()
  (let* ((a 10)
         (b 20))
    (labels ((f (x) (+ x 1))
             (g (x) (* x 2)))
      (print (f (g a)))
      (print (g (f b))))))
