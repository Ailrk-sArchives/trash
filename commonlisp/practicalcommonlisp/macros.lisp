;; chapter 7, some constructs

;; show the side effect.
(defmacro show (val)
  `(progn
     (format t "before: ~a~%" x)
     ,val
     (format t "after: ~a~%~%" x) nil))

;; our own progn
;; evaluate first n-1 forms, and return the last one.
(defun my-progn (&rest forms)
  (subseq forms (1- (length forms)))
  (car (last forms)))

(defun use-progn (bool)
  (let* ((x 10)
         (y 20))
    (if bool
        (progn
          (show (setf x (1+ x)))
          (show (incf x))
          (show (rotatef x y))
          (show (shiftf y x 100))))))


(defmacro my-when (condition &rest body)
  `(if ,condition (progn ,@body)))

(defmacro my-unless (condition &rest body)
  `(if (not ,condition) (progn ,@body)))


;; note backquotes ` are just short hand for creating list.
;; everything you write under defmacro will be executed at the
;; compile time, and the returned list will be inserted into the AST.
(defmacro set-them-* (a b) (list 'setf a 10 b 20))
(defmacro set-them (a b) `(setf a 10 b 20))

;; traverse_ on a list
(defun play-dolist ()
  (dotimes (x 10) (format t "from do time: ~a~%" x))
  (dolist (x '(1 3 5 7 8 9 11 13))
    (format t "from do list: ~a~%" x)
    (if (evenp x) (return)))) ; return break out of the loop

(defun make-id-square-matrix (n)
  (dotimes (x n)
    (dotimes (y n)
      (let ((val (if (= x y) 1 0)))
        (format t "~a " val)))
    (format t "~%")))

(defun make-caley-table (as op)
  (dolist (x as)
    (dolist (y as)
      (format t "~a " (funcall op x y)))
    (format t "~%")))

(defun mod-add-n (x y n) (mod (+ x y) n))

(defun range (n) (loop for i from 0 below n by 1 collect i))

(defun make-zn-caley-table (n)
  (make-caley-table (range n) (lambda (x y) (mod-add-n x y n))))

;; (do (variable definition) (end-test-form result form) statements)
;; n: 0 cur: 0 next: 1
;; n: 1 cur: 1 next: 1
;; n: 2 cur: 1 next: 2
;; n: 3 cur: 2 next: 3
;; n: 4 cur: 3 next: 5
;; n: 5 cur: 5 next: 8
;; n: 6 cur: 8 next: 13
;; n: 7 cur: 13 next: 21
;; n: 8 cur: 21 next: 34
;; n: 9 cur: 34 next: 55
;; 55
(defun play-do-1-fib ()
  (do ((n 0 (1+ n)) ; (var init-form step-form)
       (cur 0 next)
       (next 1 (+ cur next)))
      ((= 10 n) cur)
      (format t "n: ~a cur: ~a next: ~a ~%" n cur next)))

;; be careful that all binding in the step form will
;; use the value from the last step.
(defun fib (n)
  (do ((i 0 (1+ i))             ; couter
       (cur 0 next)             ; cur is previous of next
       (next 1 (+ cur next)))   ; new next is cur + next
      ((= n i) cur)))

(defun play-do-2 ()
  (do ((i 0 (1+ i)))    ; step fun
      ((>= i 4))        ; end test form
      (print i)))


;: mighty loop
;: simple loop, it's just an infinite loop
(defun loopy ()
  (let ((ti (+ 3 (get-universal-time))))
    (loop
      (when (> (get-universal-time) ti)
        (return))
      (format t "Waiting ~%")
      (sleep 1))))

;; loop on list and perform arbitrary statements.
(defun loop-and-do ()
  (loop for i from 1 to 10
        do (print i)))

;; extended loop

;; these two do the same thing
;; lisp do is basically a mutation based loop
;; structure.
(defun do-and-loop ()
  (do ((nums nil)   ; mutation right there ew.
       (i 1 (1+ i)))
      ((> i 10) (reverse nums))
      (push i nums))
  (loop for i from 1 to 10 collecting i))

;; you have different semantics for loop...
(defun loop-de-loop () (loop for i from 1 to 10 collecting i))

(defun loop-de-loop-1 ()
  (loop for x from 1 to 10 summing (expt x 2)))

;; how many vows are there?
(defun loop-de-loop-2 ()
  (loop for x across "the quick brown fox jumps over the lazy dog"
        counting (find x "aeiou")))

;; it's the same as fib
(defun loop-de-loop-3-fib (n)
  (loop for i below n
        and a = 0 then b
        and b = 1 then (+ b a)
        finally (return a)))

;; chapter 8
(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number)
          never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))

;; backquote is nice, you can write the code without
;; quote everything.
(defmacro do-primes-1 ((var start end) &body body)
  `(do ((,var (next-prime ,start)
              (next-prime (1+ ,var))))
       ((> ,var ,end))
       ,@body))

;; an comparison between quotes and backquotes.

(defun quotes-and-backquotes ()
  (let ((a (lambda (x y) (+ x y)))
        (c 10))
    (list
      `(a (+ 1 2) c)
      (list 'a '(+ 1 2) 'c))
    (list
      `(a ,(+ 1 2) c)
      (list 'a (+ 1 2) c))
    (list
      `(a (list 1 2) c)
      (list 'a '(list 1 2) 'c))))

;; don't write leaky abstraction!
;; 1. use gensym to generate unique symbol.
;; 2. make sure only evaluate each subform once.
;; 3. make evaluation order the same as the parameter order.

(defmacro do-primes-2 ((var start end) &body body)
  (let ((end-value (gensym)))
    `(do ((,var (next-prime ,start)
                (next-prime (1+ ,var)))
          (,end-value ,end))
         ((> ,var ,end-value)) ,@body)))

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
