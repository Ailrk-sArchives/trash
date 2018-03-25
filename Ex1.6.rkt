;; Why Alyssa's new-if will cause problem in sqrt-iter?
#lang racket
;; User Newton's method to estimate square root.

;iter method
(define (square x)
  (* x x))
(define (average x y)
  (/ (+ x y) 2))
(define (improve guess x)
  (average guess (/ x guess)))
(define (good-enough guess x)
  (< (abs (- (square guess) x)) 0.000000000000001))

(define (sqrt-iter guess x)
  (if (good-enough guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (square-root x)
  (sqrt-iter 1.0 x))

;; define new-if

(define (new-if pre-clause then-clause else-clause)
  (cond (pre-clause then-clause)
        (else else-clause)))
;; this new-if is a function, which means it will follow applicative order
;; and evaluate parameters first. In sqrt-iter, it will evaluate the recursive
;; part (sqrt-itetr ...) which will never give a answer.

;; However, the build-in if follows normal order, which helps to avoid this 
;; recursive parameter problem.
(display (if (= 4 5) "yes" "No"))(display "\n")
(display (square-root 2)) (display "\n")
