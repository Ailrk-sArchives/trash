;;;; imperative programming in common lisp

;;; common lisp has so many features for explicit mutation
;;; that it really shoundn't be regarded as a functional programming
;;; language.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; set place

;;; setq:
;;;   simply set the value binds to the symbol
(let ((x 1))
  (setq x 10))

;;; setf:
;;;   generic set macro. It will call setq when the place is a symbol,
;;;   otherwise it will invoke specific implementation for different
;;;   place forms.
(let ((x 1)
      (xs (list 1 2 3)))
  (setf (elt xs 2) 10
        x 10)
  `(,x ,(caddr xs)))

;;; setf is generic, we can expand places it accepts

(defun set-last (xs new-val)
  "set the last element of a list to new-val and
   return the modified list."
  (labels ((set-last-* (xs new-val head)
             (if (eql (cdr xs) nil)
               (progn
                 (setf (car xs) new-val)
                 head)
               (set-last-* (cdr xs) new-val head))))
    (set-last-* xs new-val xs)))

(let ((xs '(1 2 3)))
  (set-last xs 5))

;; register the set-last function to setf with setter last-th
(defsetf last-th set-last)
(let ((xs  '(1 2 3 4)))
  (setf (last-th xs) 10))

;;; side: set
;;;   Note this is totally different thing.
;;;   For setq, there are two objects
;;;   involed: sym -> object
;;;   But beause symbols themselves are objects too, all objects have
;;;   value cells, so technically one can store value in the
;;;   symbol's value cell. set is for that purpose.
;;;   set is deprecated.

(let ((x 'sym))
  (set x 10)
  `(,x ,(symbol-value x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; block structure

;;; One of the most prominent feature of imperative programming
;;; is to use blocks to separate different chunks of computations.
;;; In structure programming, we have the operator `;` that chains
;;; statements together, and perform one after another in order.

;;; Because common lisp is an expression based language, it simulate
;;; blocks with expressions too.


;;; progn
;;;   simply sequencing expression one after another. The whole
;;;   expression return what the last expression evalutes to.

(progn
  (setq a nil)
  (setq b nil)
  'here)


;;;

;;; block
;;;   a block can also sequence expressions. In addition one can
;;;   perform earily return by jumping out of a block with return-from

(block alpha
       (setq a nil)
       (setq b nil)
       (return-from alpha)
       'here)   ;; this will never be executed


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; transfering control flows
;;; This can be done in any languages, implemented with
;;; cps or go as the basis.
;;; In a imperative style, abitrary transferring of control flow
;;; is not uncommon and can be a very useful tool.

;;; Structure programming doesn't support goto, because goto
;;; transfer control flow to arbitrarily so the program behavior
;;; can no longer be anaysed by hoare logic.

;;; Primitive goto transfer the current instruction to any other
;;; instruction without caring about the concequence.

;;; Jumping from one location to another without and restruction is very
;;; problematic. A piece of code can have set up some context, required
;;; some resources, and when some computation is done those context
;;; and resources are suppose to be teared down. If we can jump away
;;; in the middle of this process, there is no longer guarantee that we
;;; wil release those resources, causing leaky program.

;;; So in most practical languages an long jump of control flow usually
;;; come with stack unwinding. In most exception handling system, if an
;;; exception is happend in the middle of somewhere, the stack is unwund
;;; before it transfer the control to the handler. Meaning, content in
;;; the current stack and all resources allocated will be released.

;;; In C++, even stack unwinding is not enough to make sure the memory
;;; safety. If we new a memory and free it at the end of a block, but an
;;; exception is thrown before we can call the destructor, the pointer to
;;; the memory will be destroyed because it's in the stack, but the
;;; allocated memory will just be there.
;;; The real problem is the arbitrary tranferring of control violates
;;; RAII, which is what the languged is designed for.

;;; In a GCed language, stack unwinding is easier to do because if we
;;; transferred away from a block, handlers on the stack will be destroyed,
;;; so unreachble heap memory will be detected at the next gc cycle and get
;;; swept away. But this doesn't save us from close external resources like
;;; files and sockets.


;;; code in unwind-project will be executed no matter
;;; when it's jumped out.
(block alpha
       (setq a nil)
       (block beta
              (setq b nil)
              (setq c nil)
              (format t "In beta~%")
              (unwind-protect
                (return-from alpha)
                (print "still executed")))  ;; this is still executed
       (setq d nil)
       (format t "End of alpha"))   ;; this will never be executed

;;; side note: unwind-protect
;;;   protects against all attempts to exit from the protected from,
;;;   including go, handler-case, ignore-errors, return-from, etc..
;;; side note: return-from
;;;   immediately return from the from to desginated block, unwind
;;;   the stack.

(block alpha
       (with-open-file (in "file.txt" :direction :input)  ;; unwind-protect
         (loop for n = (read-char in)
               with i = 0
               when (< i 100) do
               (progn
                 (incf i)
                 (if (= i 10) (return-from alpha))
                 (format t "read a char: ~a ~%" n)))))

;;; catch and throw is another mechanism we can use
;;; it's a simple version of condition system.
(defun fn-a ()
  (catch 'fn-a
         (print 'before-fn-b-call)
         (fn-b)
         (print 'after-fn-b-call)))

(defun fn-b ()
  (print 'before-fn-c-call) (fn-c) (print 'after-fn-c-call))

(defun fn-c ()
  (print 'before-throw) (throw 'fn-a 'done) (print 'after-throw))

(fn-a)
