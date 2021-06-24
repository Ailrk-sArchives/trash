;;;; form macro

;; takes a form with name substituted

(defmacro with-name ((s name) &body body)
  `(progn
     (format t "~a" ,s)
     (format t "~a" ,name)
     (let ((x ,(car (car (cdr body))))
           (y ,(cdr (car (cdr body)))))
       (format t "~a" x)
       (format t "~a" y)
       (values x y))))


(defmacro listit ((a b c) (d e))
  `(list ,a ,b ,c ,d ,e))
