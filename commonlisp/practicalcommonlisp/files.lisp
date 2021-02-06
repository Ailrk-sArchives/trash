; read
;; common lisp has a very complicated stream system.
;; (not really, but the api is hard to follow up)
;; I'm sure if you are already very familiar with the
;; mechanism it will be very nice to use, but there are
;; simply too many details exposed.

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

(with-output-to-string (out)
  (format out "output this string to out")
  out)


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

; an example implementation of getenv

(defun my-getenv (name &optional default)
  "Obtains the current value of the POSIX environment variable NAME."
  (declare (type (or string symbol) name))
  (let ((name (string name)))
       (or #+abcl (ext:getenv name)
           #+ccl (ccl:getenv name)
           #+clisp (ext:getenv name)
           #+cmu (unix:unix-getenv name) ; since CMUCL 20b
           #+ecl (si:getenv name)
           #+gcl (si:getenv name)
           #+mkcl (mkcl:getenv name)
           #+sbcl (sb-ext:posix-getenv name)
           default)))
