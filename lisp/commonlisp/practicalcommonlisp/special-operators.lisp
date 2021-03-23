;; use labels to define local recursive function
(defun collect-leaves (tree)
  (let ((leavse ()))
    (labels ((walk (tree)
               (cond
                 ((null tree))
                 ((atom tree) (push tree leaves))
                 (t (walk (car tree))
                  (walk (cdr tree))))))
      (walk tree))
    (nreverse leaves)))

;; tagbody and goto
(let ((x 10))
     (tagbody
      top
      (decf x)
      (print "hello")
      (if (> x 0) (go top))))

;; return from with a named function
(defun fooi (x)
  (print "good")
  (return-from foo t)
  nil)

;; return to named block
(block b
       (print "here")
       (return-from b)
       (print "after"))

;; return to nil block
(block nil
  (print "here")
  (return)
  (prin1 "after"))

;; jump around
(tagbody
  a (print 'a) (if (zerop (random 2)) (go c))
  b (print 'b) (if (zerop (random 2)) (go a))
  c (print 'c) (if (zerop (random 2)) (go b)))

;; knuth's algorithm-s
(defun algorithm-s* (n max)
  (let (seen
        selected
        u
        (records ()))
       (tagbody
        s1
        (setf seen 0)
        (setf elected 0)
        s2
        (setf u (random 1.0))
        s3
        (when (>= (* (- max seen) u) (- n selected)) (go s5))
        s4
        (push seen records)
        (incf selected)
        (incf seen)
        (if (< selected n)
            (go s2)
            (return-from algorithm-s* (nreverse records)))
        s5
        (incf seen)
        (go s2))))

(defun algorithm-s (n max)
  (loop for seen from 0
        when (< (* (- max seen) (random 1.0)) n)
        collect seen and do (decf n)
        until (zerop n)))

;; unwinding the stack
(defun baz (fn)
  (format t " Entering baz~%")
  (funcall fn)
  (format t " Leaving baz~%"))

(defun bar (fn)
  (format t " Entering bar~%")
  (baz fn)
  (format t " Leaving bar~%"))

(defun foo ()
  (format t " Entering foo~%")
  (block a
         (format t " Entering Block~%")
         (bar (lambda () (return-from a)))
         (format t " Leaving Block ~%"))
  (format t " Leaving foo~%"))

;; result:
;  Entering Block
;  Entering bar
;  Entering baz
;  Leaving foo
;; The stack is unwind once you call return-from, and
;; everything after that block get discarded.

;; unwind-protect makes sure certain code always run even
;; if there is a stack unwinding.
;; (unwind-protect protected-form cleanup-form)

;; + just discard multivalue
(funcall #'+ (values 2 3) (values 4 5))

;; will treat everything as a list
(multiple-value-call #'+ (values 1 2) (values 3 4))

(multiple-value-bind (x y) (floor 30 20)
  (+ x y))

;; value-list is the inverse of the value list.

;;;; Eval when
;; control when a

;; load and compile-file
;; load: laod all top levle forms. The order of side effects will affect
;; subsequent loading.
;; compile-file: compile file into fasl, usually doesn't load, only load case
;; of keeping consistentcy

(defparameter *s* "a")
;; this will be load at compile time
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *s*  "PP"))
