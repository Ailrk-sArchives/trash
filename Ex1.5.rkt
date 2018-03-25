;; A method to test if the interpreter is applicative order or
;; normal order
#lang racket
(define (p) (p))
(define (test x y)
  (if (= x 0)
    0
    y))

;test
(test 0 (p))

;; So the definition of (p) is another (p), which is a recursive define
;; If the interpreter is applicative, it will evaluate 0 ,(p) when it call
;; (test 0 (p)). However, the expansion of (p) here is another (p), so the
;; program will stuck at this point, keep evaluate (p) forever.

;; On the othe hand, normal order will expand the whole expression first, then
;; further expand each sub clause. Since x = 0, This funtion will return 0
