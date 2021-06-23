;;;; read macro

;;; lisp macros works at two stage: read time and compile time.
;;; read macro is more like macro in a c preprocesser sense as it
;;; works on plain text.
;;; macro, instead, works on parsed ast.

;;; reader macro involves using the read function
;;; read : optional input steram
;;;     -> optional eof error predicate  : bool
;;;     -> optional eof value : a
;;;     -> optional recursive predicate :  bool

;;; when implementing reader macros, recursive-p should always be true

;;; stream primer
;;; first try to understand where does things come from
(defun stream-primer ()
  (let ((ss (make-string-input-stream "1 one ")))
    (check-type ss stream)
    (when (input-stream-p ss)
      (list (read ss nil nil)   ;; read 1, skip space
            (read ss nil nil)   ;; read one, skip space
            (read ss nil nil)   ;; points to the nil, only nil to read
            (read ss nil nil)))))

;;; quote
;;; simplest macro that translate form '<a> to (quote a)
;;; we use ^ instead of '
(defun single-quote-reader (stream char)
  (declare (ignore char))
  ;; construct (quote ...)
  (list (quote quote) (read stream t nil t)))

;;; set-marco-character makes the lisp reader assoicate the char
;;; with the reader. Anything after ^ will be processed at read time.
(set-macro-character #\^ #'single-quote-reader)

;;; the process of running a customized reader macro:
;;; > ^asd
;;; 1. reader read "^asd",
;;; 2. reader find ^, it's defined, so calls the corresponding function
;;; 3. single-quote-reader is called, which in turn calls read again.


;;;; json

(defconstant +left-bracket+ #\[)
(defconstant +right-bracket+ #\])
(defconstant +left-brace+ #\{)
(defconstant +right-brace+ #\})
(defconstant +comma+ #\,)
(defconstant +colon+ #\:)

(defun read-next-object (separator
                         deliminator
                         &optional (input-stream *standard-output*))
  (check-type separator char)
  (check-type deliminator char)
  ;; define two short hands
  ;; : peek-char peek a char from the input stream
  ;; : read-char consume a char from the input stream
  (flet ((peek-next-char () (peek-char t input-stream t nil t))
         (discard-next-char () (read-char input-stream t nil t)))
    (if (and deliminator (char= (peek-next-char) deliminator))
      (progn
        (discard-next-char)
        nil)
      (let* ((object (read input-stream t nil t))
             (next-char (peek-next-char)))
        (cond
          ((char= next-char separator) (discard-next-char))
          ((and deliminator (char= next-char deliminator)) nil)
          (t (error "Unexpected nextchar: ~S" next-char)))
        object))))

(defun read-left-bracket (stream char)
  (declare (ignore char))
  ;; readtable: map between character and their reader macro (can be nil)
  ;; copy-readtable simply copy the current readtable
  ;; this is a example of using dynamic variable.
  ;; it copy the global config, and do some modification on it.
  (let ((*readtable* (copy-readtable)))
    (set-macro-character +comma+ 'read-separator)
    ;; see how pros use loop
    (loop
      for object = (read-next-object +comma+ +right-bracket+ stream)
      while object
      collect object into objects
      finally (return `(vector ,@objects)))))

(set-macro-character +left-bracket+ 'read-left-bracket)
