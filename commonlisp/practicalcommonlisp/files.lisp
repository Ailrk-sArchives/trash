; read
(let ((in (open "db1" :if-does-not-exist nil)))
  (when in
    (format t "~a~%" (read-line in))
    (close in)))

(let ((in (open "./file.txt" :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
          while line
          do (format t "~a~%" line))
    (close in)))

(let ((in (open "./file.txt")))
  (dotimes (n 3) (read-line in))
  (format t "~a~%" (read in))   ; read read s-expr directly
  (format t "~a~%" (read in))   ; and return the list object
  (format t "~a~%" (read in)))

; read and print helps you to serialize and deserialze
; easily.
(read (print '(1 2 3)))

; context manager for files
; nil is almost necessary for read-line...
(with-open-file (in "./file.txt")
  (loop for line = (read-line in nil)
        while line
        do (format t "~a~%" line)))

(with-open-file (stream "db2" :direction :output)
  (format stream "Some text."))

; how does the context manager work
(defun try-it ()
   (unwind-protect
     (return-from try-it) ; doesn't really return here
     (print 'iamhere)))

(defun try-it-unprotected ()
  (return-from try-it-unprotected)
  (print 'iamhere))  ; this will not be printed
