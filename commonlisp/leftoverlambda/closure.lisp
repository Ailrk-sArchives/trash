;;;; This chapter talkes bout lexical scoping and closure

;;;; environment and extent ;;;;
;; an environment with temporary extent will last temporarily.
;; environment with indefinite extent will always being able to be referred to.

(defun parse-float (input)
  (with-input-from-string (s input)
    (car (loop for num = (read s nil nil)
               while num collect num))))

;; scanf in lisp
;; only takes %d, %s, %f
;; (fn &rest params)
(defun scanf (s fn)
  (labels ((read-word ()
             (with-output-to-string (s)   ;; how to get a string from a stream.
               (format s "~{~A~}"
                     (loop for c = (read-char)
                           while (not (member c '(#\Space #\Linefeed)))
                           collect c))
               s)))
    (let* ((anchors nil)
           (args nil)
           (next nil))
      (loop for c across s do
        (if (eq c #\%)
            (setf next t)
            (if next
                (progn
                  (push c anchors)
                  (setf next nil)))))
      (loop for i from (- (length anchors) 1) downto 0
            for n = (elt anchors i) do
        (cond ((eq n #\d) (push (parse-integer (read-word)) args))
              ((eq n #\f) (push (parse-float (read-word)) args))
              ((eq n #\s) (push (read-word) args))
              (t (error "illegal anchor for scanf"))))
      (apply fn (nreverse args)))))


;; works pretty well
;; this chapter uses this as an example. The poiont is you don't pass
;; pointer of variable you want to change, instead you pass a closure that
;; capture those variables.
;; this way you get a mutable reference to variable you want to modify, but
;; only within the lambda you pass in.
(let ((a nil)
      (b nil)
      (c nil)
      (d nil))
  (scanf "%d %f %f %s"
         (lambda (x1 x2 x3 x4)
           (setf a x1)
           (setf b x2)
           (setf c x3)
           (setf d x4)))
  (format t "~% a is now ~a~%" a)
  (format t "~% b is now ~a~%" b)
  (format t "~% c is now ~a~%" c)
  (format t "~% d is now ~a~%" d))

;; I think closure was still a quite new idea while the book as written, but now
;; anybody should be able to get this easily.

;; It's just what's the actual benefit you get?
;; comparing with passing a point to get assigned, passing a closure is more
;; flexible as you can add additional logic to it.
;; It's not necesary that only pass the value to modify, as the closure can
;; capture any other variables it requres to work with.
