;; describe the behavior of the following procedure
#lang racket

(define (a-plus-abs-b a b)
  (if (> b 0) + -) a b)

(display (a-plus-abs-b 3 -4))

;; It will check if b is positive and negative, and assign different
;; operator accroding to the result.

;; It shows the difference between procedure and data is very blur here.
