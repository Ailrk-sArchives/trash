;;;; pattern matching with trivia
;;; it's weird common lisp doesn't support pattern matching
;;; by default. But at least the package is easy to get.

(use-package :trivia)

(match `(1 2 3)
       ((cons x y)
        (print x)
        (print y)))

(match `(something 2 3)
       ((list a b _)
        (values a b)))

(match `(something 2 3)
       ((list* a b)
        (values a b)))

(match #(1 2 3)
       ((vector _ x _)
        (print x)))

;; xx* pattern is flexible in length
(match #(1 2 3 4)
       ((vector* _ x _)
        (print x)))

;; there are three style to match a struct
(defstruct foo bar baz)
(match (make-foo :bar 0 :baz 1)
       ((foo :bar a :baz b)
        (values a b))
       ((foo (bar a) (baz b))
        (values a b))
       ((foo bar baz)
        (values bar baz)))

(match (list 2 5)
       ((guard (list x y)
               (= 10 ( * x y)))
        :ok))
