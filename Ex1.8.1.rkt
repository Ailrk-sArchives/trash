;; Block structure to rewrite sqrt
#lang racket

;; free variable for square-root
(define (square x)
  (* x x))
(define (average x y)
  (/ (+ x y) 2))

;; x is free variable to all codes within the procedure (square-root)
;; while it is bound variable to exteral environment

;; The code within square-root is a scope, its internal variable is
;; invisible to the external code, however, variable outside the scope
;; can be used by it directly.

;; This is called lexical scoping

;; The benefit to write code in the block structure is it helps to hide
;; details, avoid naming conflicts, and abstract details.

(define (square-root x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(display (square-root 9))
