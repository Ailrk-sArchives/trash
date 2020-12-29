; early chapters

(defun hello-world ()
  (format t "Hello world"))

(defun foo (&key a (b 20) (c 30 c-p)) (list a b c c-p))

(defun gcd- (a b)
  (if (= b 0)
      a
      (gcd- b (mod a b))))


; chapter 3 cd database
; property list with keyword symbol
(defparameter *propertylist* (list :a 1 :b 2 :c 3))
(getf *propertylist* :a)

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

; *xx* is the name convention for global variables
(defvar *db* nil)
(setf *db* nil)

(defun add-record (cd) (push cd *db*))

; format: ~a  :  aesthetic directive
;         ~10t:  10 space
;         ~%  :  emit 1 new line
;         ~{ }~ : process format string multiple times
(defun dump-record ()
  (dolist (cd *db*)
    (format t "~5tCD Records~%~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*) ;
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
    (prompt-read "Title")
    (prompt-read "Artist")
    (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
    (y-or-n-p "Ripped")))

(add-record (make-cd "sunny" "potsu" 100 t))
(add-record (make-cd "banana with nuts" "potsu" 100 nil))
(add-record (make-cd "bossa uh" "potsu" 100 t))
(add-record (make-cd "misty" "ella fizgerald" 100 nil))
(add-record (make-cd "ain't misbehavin" "joe pass" 100 nil))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename
                      :direction :input)     ; default is reading
    (with-standard-io-syntax
      (setf *db* (read in)))))

; keyword parameter and supplied-p
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
        (if title (equal (getf cd :title) title) t)
        (if artist (equal (getf cd :artist) artist) t)
        (if rating (equal (getf cd :rating) rating) t)
        (if ripped-p (equal (getf cd :ripped) ripped) t))))


