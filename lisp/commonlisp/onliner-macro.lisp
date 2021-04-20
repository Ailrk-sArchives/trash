;; there are a lot of restructions for mutation in common lisp.
;; like if you want to make a form setable you need to have the
;; corresponding place define with (DEFSETF).
;; and because it's macro, a value (elt x x) returned from a function
;; won't work.
;; But sometimes you need to refer to one value again and again.
;; like if I'm working with (elt x 1), I will need to address it multiple
;; time in a body.

(defun foo ()
  (let ((xs '(1 2 3 4 5 6)))
    (macrolet ((3rd () `(elt xs 3)))
      (setf (3rd) 10)
      xs)))

(foo)
