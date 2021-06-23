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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Output stream specific operations



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

