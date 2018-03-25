;; Newton's method for cube-root:
;; ((x/y)^2 + 2*y) / 3
;; Use this formula to implement a cube-root procedure analogous to the square-root.
#lang racket

(define (cube x)
  (* x x x))

(define (square x)
  (* x x))

(define (cube-root-iter guess x)
  (if (good-enough? guess x)
    guess
    (cube-root-iter (improve guess x) x)))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess))  3))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) (* guess 0.001)))

(define (cubert x)
  (cube-root-iter 1.0 x))

(display (cubert 27))



