;; good-enough? test is not effective for finding the square roots of very
;; small numbers. Also, in real computers, arithmetic opeations are almost 
;; performed with limited precision. This makes our test inadequate for very
;; large numbers. Explain these statements, with example showing how the test fails
;; for small and large changes from one iteration to the next.

;; An alternative strategy for implementing good-enough? is to wathch how guess changes
;; from one iteration to the next and to stop when the change is a very small
;; fraction of the guess. Design a square-root procedure that uses this kind of 
;; end test. Does this work better for small and large numers?
#lang racket
;; Ans

;; Because the guess is a fixed value 1.0, if x is a very small number:
;; (square-root 0.001)
;; (/ x guess) will become a very large number.
;; so (average x y) will become a very large too.
;; which means (good-guess guess x) will take a large number iteration to return true.

;; On the other hand, if the x is very large:
; (square-root 100000)
;; (/ x guess) will become a very small number.
;; so (average x y) will become almost y /2
;; which means number in good-guess almost not change

;; To solve this problem, a flexible initial guess is required.
(define (square x)
  (* x x))
(define (average x y)
  (/ (+ x y) 2))
(define (improve guess x)
  (average guess (/ x guess)))


(define (good-enough? guess x)
  ;; Compare the difference between improved guess and previous guess
  ;; this value is small means there is not much to improve
  ;; The scale is related to size of guess
  (< (abs (- (improve guess x) guess)) (* guess 0.00000001)))


(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))


(define (square-root x)
  (sqrt-iter 1.0 x))


(display (square-root 10000)) (display "\n")
(display (square-root 0.00001)) (display "\n")



