;;;; some common lisp practices.

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (prin1 a s))))

(defun flattern (xs)
  (labels ((rec (xs acc)
             (cond ((null xs) acc)
                   ((atom xs) (cons xs acc))
                   (t (rec (car xs) (rec (cdr xs) acc))))))
    (rec xs nil)))

(flatten '((12 (8 83 (2 (2))) 4 (2 34 1) (2 3 (2 3 4)))))
