;; Define a procedure that takes three numbers as arguments and return
;; the sum of the two larger numbers
#lang racket
(define (square x) (* x x))

(define (sumsquares x y) (+ (square x) (square y)))

(define (sumlargest a b c)
  (cond
    ((and (>= a c) (>= b c)) (sumsquares a b))
    ((and (>= b a) (>= c a)) (sumsquares b c))
    ((and (>= a b) (>= c b)) (sumsquares c a))))


;test
(display (sumlargest 1 1 1)) ;Value 2
(display (sumlargest 1 2 3)) ;Value 13
(display (sumlargest 0 0 0)) ;Value 0
(display (sumlargest 5 3 1)) ;Value 34
