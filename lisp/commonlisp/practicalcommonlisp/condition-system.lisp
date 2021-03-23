;; condition system
;; combinations of exceptions and warnings
;;
;; In a java/python like exception system, each time a code throws
;; an exception, the stack will unwind, and the call site will be destoried.
;; So even if the caller get signaled that some exception happened, it can't
;; do much to fix it because the callee no longer exists.
;;
;; But with common lisp conditon system, we can have keep the callee intact,
;; while choose what handler to call to fix the exception. As if you can
;; freeze the time and choose what to do.
;;
;; Three components of a condition system
;; 1. signaling a condition
;; 2. handling a condition
;; 3. recover from a condition
;; You can handle exceptions at lower level functions, not like in languages like
;; python, error handlers are all defined in higher level functions.


;; (in-package :error-handling)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define two errors
(define-condition file-io-error (error)
  ((message :initarg :message :reader message)))

(define-condition another-file-io-error (error)
  ((message :initarg :message :reader message)))

;; simulate an io operation that might fail.
;; it throws like a normal language with try/catch, there is
;; no condition handler defined with the code, all error recovering
;; code are from the caller.
(defun fake-io (&key (fail nil fail-p) (message "Error!"))
  (cond
    ((not fail-p)
     (if (evenp (random 100))
         (error 'file-io-error :message "message")
         "success"))
    (fail (error 'another-file-io-error :message "message"))
    (t "success")))

;;;; flush the io buffer:
;; finish-output, force-output, and clear-output

;;;; lisp has muli value output
;; to obtain multiple values as a list you can wrap the function in
;; multiple-value-list. Similary to destruct multiple values reutrned
;; you can use multiple-value-bind to bind values with a name.

;; define a restart function
(defun read-new-value ()
  (format t "Enter a new value: ")
  (force-output)
  (multiple-value-list (eval (read))))

;; use restart when error happens.
;; these cases will be added into the debugger options so you can invoke.
;; (restart-case (form) (restart1) (restart2) ...)
(let ((fail t))
     (restart-case (fake-io :fail fail)  ;; expression to run
                   ;; first handler we define
                   (retry-without-errors (new-fail)
                                         :report "Pass in a fail value"
                                         :interactive read-new-value (setf fail new-fail)
                                         (fake-io :fail fail))
                   ;; second handler simply do nothing
                   (do-nothing ()
                               :report "don't handle the error"
                               "done with it!")))
(fake-io)
(fake-io :fail t)
(fake-io :fail nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; we want to define handler to automatically handle
;;;; handler-case behaves like try-catch in other languages.
;; catching any conditions
(handler-case (/ 3 0)
  (error (c) (format t "We caught a condition ~&")
         (values 0 c)))

;; another way to catch all conditions with t
(handler-case
  (progn
    (format t "This won't work~&")
    (/ 3 0))
  (t (c)
     (format t "Got a condition ~a~%" c)
     (values 0 c) ) )

;; catching specific conditions
(handler-case (/ 3 0)
  (division-by-zero (c)
                    (format t "caught a division by zero condition  ~a~%" c)
                    (values 0 c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; we define a new condition here
(define-condition on-zero-denominator (error)
  ((message :initarg :message :reader message)))

;; reciprocal will decide what conditon to throw, also it defines
;; some handler for that condition.
(defun reciprocal (n)
  (restart-case
    (if (/= n 0)
        (/ 1 n)
        (error 'on-zero-denominator :message "can't divide by zero"))
    (return-zero () :report "Just return 0" 0)
    (return-value (r) :report "Return another value" r)
    (recalc-using (v) :report "recalculate" (reciprocal v))
    (return-nil () nil)))

;; in this function we choose to ignore condition
;; but we also provide another possible way to handle any possible
;; condition, namely `just continue`
(defun list-of-reciprocals (array)
  (restart-case
    (mapcar #'reciprocal array)
    (just-continue () nil)))''

;; here we 'bind on-zero-denominator with a lambda handler.
;; in the handler it calls the lower level handler provided by reciprocol.
;; in this case, we choose to just return 0.
(defun print-reciprocals (array)
  (handler-bind
    ((on-zero-denominator
       #'(lambda (c)
           (format t "error signaled: ~a~%" (message c))
           (invoke-restart 'return-value 0))))
    (let (r)
      (setf r (list-of-reciprocal array))
      (dolist (x r)
        (format t "Reciprocal: ~a~%" x)))))
