;chapter 7, some constructs
; show the side effect.
(defmacro show (val)
  `(progn
     (format t "before: ~a~%" x)
     ,val
     (format t "after: ~a~%~%" x)
     nil))

; our own progn
; evaluate first n-1 forms, and return the last one.
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

;; traverse_ on a list
(defun play-dolist ()
  (dolist (x '(1 3 5 7 8 9 11 13))
    (print x)
    (if (evenp x) (return)))) ; return break out of the loop
