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
