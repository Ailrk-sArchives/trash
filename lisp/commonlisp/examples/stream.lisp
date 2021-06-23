;;;; stream
;;; stream is a very important abstraction in common lisp
;;; it's used for almost all sort of IO operations.

;;; Stream:
;;;   a stream is an object that can be used with an input or
;;;   output function to identify anapproriate source or sink of
;;;   characters or bytes for that operations.

;;; Stream designator:
;;;   A designator for a stream that is an object that denotes a
;;;   stream.
;;;   deginator can be one of:
;;;     t: denoting  *terminal-io*
;;;     nil: denoting *standard-input*
;;;     *standard-output*: standard output

;;; A stream can be either a character stream or a binary stream.
;;; base on its direction it can be a input stream or an output stream.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check if an obejct is a stream
(streamp *standard-input*)
;; is the same as
(typep *standard-input* 'stream)

;; usually we have two streams from on type: input and output
;; we can use following predicates to test if it's an
;; input or output stream
(open-stream-p *standard-input*)
(input-stream-p *standard-input*)
(output-stream-p *standard-output*)

(stream-element-type *standard-input*)

;; a generic context manager
(with-open-stream (s (make-string-input-stream "1 2 3 4"))
  (+ (read s) (read s) (read s) (read s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input stream specific operations
;;; problems a input stream system needs to solve:
;;; 0. where are things come from?
;;; 1. what type is read.
;;; 2. how many value is read at once?
;;; 3. should I consume the input or just look at the input?
;;; 4. input streams are designed for reading lisp and s-expression,
;;;    in that case all white spaces are noise.
;;;    but what if we actually want to read plain string?
;;; 5. probe the status of the stream. (has values? empty?)

;;; the most important one is read
(defun slash-reader (stream char)
  (declare (ignore char))
  `(path . ,(loop for dir = (read-preserving-whitespace stream t nil t)
                  then (progn (read-char stream t nil t)
                              (read-preserving-whitespace stream t nil t))
                  collect dir
                  while (eql (peek-char nil stream nil nil t) #\/))))
(set-macro-character #\/ #'slash-reader)

;;; clears any available inputs
(defun read-sleepily (&optional (clear-p nil) (zzz 0))
  (list (progn (print '>) (read))
        (progn (print '>)
               (if zzz (sleep zzz))
               (if clear-p (clear-input))
               (read))))

;;; listen:
;;;   return true if a char is immediately available.
;;;   otherwise return false
(progn (unread-char (read-char))
       ;; this kinda parallel the operation (not really)
       ;; it's because to construct the list lisp needs to wait
       ;; for both side effects been performed.
       (list (listen) (read-char)))

;;; variations of read
(read-line (make-string-input-stream "line1
                                     line 2")
           nil  ; eof-error-p
           nil  ; eof-value
           nil) ; recursive-p

;;; read a char from the stream
(with-input-from-string (is "123")
  (do ((c (read-char is) (read-char is nil 'the-end)))
      ((not (characterp c)))
      (format t "~S" c)))

(with-input-from-string (is "  1 2 3 4 5")
  (format t "~S ~S ~S"
          (peek-char t is)      ; skip white spaces
          (peek-char #\4 is)    ; skip til 4 is found
          (peek-char nil is)))  ; no skip.

(with-input-from-string (is "0123")
  (dotimes (i 6)
    (let ((c (read-char is)))
      (if (evenp i)
        (format t "~&~S ~S~%" i c)
        (unread-char c is)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Output stream specific operations

;;; Problem needs to be solved by output stream system
;;; 0. Where are we writing to?
;;; 1. what type are we writing?
;;; 2. what's the format of the output?
;;; 3. how to set pretty printer for output?
;;; 4. how many stuffs to write at once?

;;; write the printed representation to the output stream
;;; write is the core function to use, tehre are other short hands
;;; specialize for specific situations.
(princ "asd" *standard-output*)
(princ "asd" t)
(pprint "asd" t)
(prin1 "asd" t)

;;; finish output
(progn (princ "am I seen?") (clear-output))

;;; write char
(with-output-to-string (s)
  (write-char #\a s)
  (write-char #\Space s)
  (write-char #\b s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; There are different types of streams
;;; string-stream is stream that holds string data types.
;;; file-stream handles io of files,
;;; also we have broad-case stream, concatenated-stream, echo stream ..

;;; string stream
(let ((ss (make-string-input-stream "1 one ")))
  (check-type ss stream)
  (list (read ss nil nil)
        (read ss nil nil)
        (read ss nil nil)
        (read ss nil nil))
  (close ss)) ;; we can close the stream at the end

;; raii for stream. close the stream automatically
(let ((ind 0))
  (with-input-from-string (s "XXX1 2 3 4"
                             :index ind
                             :start 3
                             :end 10)
    (+ (read s) (read s) (read s)))
  (format t "~a" ind))


;;; broadcast stream:
;;;   an output stream which has associated with it a set of zero or
;;;   more output stream.
;;;   it combines multiple output streams and output to all of them.

(let* ((as (make-string-output-stream))
       (bs (make-string-output-stream))
       (broadcast (make-broadcast-stream as bs *standard-output*)))
  (format broadcast "this goes to both output streams")
  (format t "the boardcast stream output to: ~%~a ~%"
          (broadcast-stream-streams broadcast))
  (concatenate 'string
               (get-output-stream-string as)
               (get-output-stream-string bs)))

;;; conctenated stream
;;;   input stream which is a composite stream of zero or more
;;;   input streams.
;;;   it's the inverse of broadcast stream

(read (make-concatenated-stream
        (make-string-input-stream "1")
        (make-string-input-stream "2")))

;;; echo stream:
;;;   a bindirectional stream that gets its input from an
;;;   associated input and sends it to an associated output
;;;   all input taken from the input stream is echoed to the
;;;   output stream

(let ((out (make-string-output-stream)))
  (with-open-stream
    (s (make-echo-stream
         (make-string-input-stream "this-is-read-and-echoed")
         out))
    (read s)
    (format s " * this-is-direct-output")
    ;; : output-stream -> string
    ;; return all the characters in string-out-stream
    (get-output-stream-string out)))
