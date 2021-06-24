;;;; path
(probe-file "foo")

;; better portability
(uiop:probe-file* "/etc/passwd")

;;;; expand a file name with ~
;; expand to user home directory
(uiop:native-namestring "~/.vim/")

(directory ".")
(directory "./*")
(directory "*.*")
(directory "*/*.*")
(directory "**/*/*.*")

;; get absolute path
(truename "../cpp/tp/sfinae.cc")

;; concat path name
(merge-pathnames (elt (directory ".") 0) #P"file.txt")

;; the order is truely obnoxious.
(merge-pathnames #P"ugly" #P"good/bad/")

;;;; create directory
;; create a folder if not exists. / is necessary
(ensure-directories-exist (uiop:native-namestring "~/temp-from-lisp/"))

(with-output-to-string (out)
  (format out "good ~a" '(1 2 3))
  out)

(defun try-it ()
  (unwind-protect
    (return-from try-it)
    (print 'imhere)))

;;;; open and close streams, input and output.
;; out as a stream
(with-output-to-string (out)
  (with-open-file (in "./demo.lisp")
    (loop with buffer = (make-array 256 :element-type 'character)
          for n-characters = (read-sequence buffer in)
          while (< 0 n-characters)
          do (progn
               (format t "buffer filled, content:  ~%")
               (format t "~a~%" buffer)
               (write-sequence buffer out :start 0 :end n-characters)))))

;; with open file take both input and output.
(with-open-file (in "./demo.lisp" :direction :input)
  (loop for line = (read-line in nil)
        while line
        do (format t "~a" line)))

(unwind-protect ()
  (with-open-file (out "./file.txt" :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (with-open-file (in "./demo.lisp" :direction :input)
      (loop for line = (read-line in nil)
            while line do (format out "~a~%" line)))))

;; treat string a input
;; peek char just peek doesn't consume
(with-input-from-string (stream "this is a string")
  (print (read-char stream))
  (print (peek-char nil stream))

  (print (read-char stream))
  (print (read-char stream))

  (print (peek-char nil stream))
  (print (peek-char nil stream))
  (print (read-char stream))
  (values))

;; get file extension
(pathname-type "~/file.lisp")
