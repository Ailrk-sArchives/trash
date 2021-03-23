;; some onlisp utils

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (prin1 a s))))

;; creating symbols
(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

;; grouping list by n
(defun group (source n)
  (if (zerop n) (error "zero length")
      (labels ((rec (source acc)
                 (let ((rest (nthcdr n source)))
                   (if (consp rest)
                       (rec rest (cons
                                   (subseq source 0 n)
                                   acc))
                       (nreverse (cons source acc))))))
        (if source (rec source nil) nil))))

(group (loop for n from 1 to 10 collect n) 3)

;; flat nested list.
(defun flattern (xs)
  (labels ((rec (xs acc)
             (cond ((null xs) acc)
                   ((atom xs) (cons xs acc))
                   (t (rec (car xs) (rec (cdr xs) acc))))))
    (rec xs nil)))
