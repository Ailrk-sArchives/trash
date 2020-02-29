#lang racket/base

#| Exercise 1.3: |#
#|  Define a procedure that takes three numbers |#
#| as arguments and returns the sum of the squares of the two |#
#| larger numbers. |#
(module basic racket/base
  (provide sum-large-two)
  (define (square x) (* x x))
  (define (sum-square a b) (+ (square a) (square b)))
  (define (sum-large-two a b c)
    (cond [(and (> a c) (> b c)) (sum-square a b )]
          [(and (> a b) (> c b)) (sum-square a c)]
          [(and (> b a) (> c a)) (sum-square b c)])))


#| Exercise 1.5: |#
#| Ben Bitdiddle has invented a test to determine |#
#| whether the interpreter he is faced with is using applicative- |#
#| order evaluation or normal-order evaluation. He defines the |#
#| following two procedures: |#

#| (define (p) (p)) |#
#| (define (test x y) |#
#| (if (= x 0) 0 y)) |#

#| Then he evaluates the expression |#

#| (test 0 (p)) |#

#| What behavior will Ben observe with an interpreter that |#
#| uses applicative-order evaluation? |#
;; Ans:
(module application-order racket/base
  (provide test)
  (define (p) (p))
  (define (test x y)
  (if (= x 0) 0 y)))
;;      In Applicative order the expression will never stop, since (p)
;;      doesn't terminates.
;;
;;      In Normal order the expression evaluates to 0.
;;      Because the expression is expanded first then eval the boolean
;;      which has a short circuit.



#| 1.1.7 Newton's method |#
(module newtons-method racket/base
  (provide average
           square
           good-enough?)
  (define (average . args) (/ (apply + args) (length args)))
  (define (square x) (* x x))
  (define (good-enough? pred guess x) (pred guess x 0.000001)))


(module sqrt-newtons-method racket/base
  (require 'newtons-method)
  (provide square-root)
  (define (sqrt-pred guess x t)
    (< (abs (- (square guess) x)) t))

  (define (square-improve guess x)
    (average guess (/ x guess)))

  (define (sqrt-iter guess x)
    (if (good-enough? sqrt-pred guess x) guess
      (sqrt-iter (square-improve guess x) x)))

  (define (square-root x)
    (sqrt-iter 1.0 x)))

#| Exercise 1.7: |#
#| The good-enough? test used in computing |#
#| square roots will not be very effective for finding the square |#
#| roots of very small numbers. Also, in real computers, arith- |#
#| metic operations are almost always performed with lim- |#
#| ited precision. This makes our test inadequate for very large |#
#| numbers. Explain these statements, with examples showing |#
#| how the test fails for small and large numbers. An alterna- |#
#| tive strategy for implementing good-enough? is to watch |#
#| how guess changes from one iteration to the next and to |#
#| stop when the change is a very small fraction of the guess. |#
#| Design a square-root procedure that uses this kind of end |#
#| test. Does this work better for small and large numbers? |#

;; Ans:
;     For small value, the fixed tolerance 0.001 might
;     not be significant enough. If x i 0.0001, 0.001 is 100%
;     larger, this will yield a huge inaccuracy.
;
;     For large value, the machine might not be able to store
;     enough digits, so at some points if the difference still
;     haven't hit the tolerance but the value generated doesn't
;     change anymore because it ignore smaller digits, then the
;     program will loop forever.
;
;     A relative tolerance change the tolerance by the delta of
;     one guess and the next. It can avoid 2 aforementioned problems.


#| Exercise 1.8: Newton’s method for cube roots is based on |#
#| the fact that if y is an approximation to the cube root of x, |#
#| then a better approximation is given by the value |#
#| x/y 2 + 2y   |#
#| -----------. |#
#|      3       |#
#| Use this formula to implement a cube-root procedure anal- |#
#| ogous to the square-root procedure. (In Section 1.3.4 we will |#
#| see how to implement Newton’s method in general as an |#
#| abstraction of these square-root and cube-root procedures.) |#
(module better-approximation-cube-root racket/base
  (require 'newtons-method) (provide cubic-root)
  (define (cubic x) (* x x x))
  (define (cubic-improve guess x)
    (average (/ x (square guess)) guess guess))

  (define (cubic-pred guess prev-guess t)
    (< (abs (- guess prev-guess)) (abs (* t guess))))

  (define (cubicroot-iter guess pre-guess x)
    (if (good-enough? cubic-pred guess pre-guess) guess
      (cubicroot-iter (cubic-improve guess x) guess x)))

  (define (cubic-root x)
    (cubicroot-iter 1.0 0 x)))


#| 1.2.1 Liear Recursion and Linear Iteration |#
(module rec-iter racket/base
  (provide factorial-rec factorial-iter)
  (define (factorial-rec n)
    (if (= n 1)
      1
      (* n (factorial-rec (- n 1)))))

  (define (factorial-iter n)
    (define (iter product count max-count)
      (if (> count max-count)
        product
        (iter (* product count)
              (add1 count)
              max-count)))
    (iter 1 1 n)))



#| Exercise 1.10: |#
#| The following procedure computes a math- |#
#| ematical function called Ackermann’s function. |#
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))
#| What are the values of the following expressions? |#
(A 1 10)
(A 2 4)
(A 3 3)
#| Consider the following procedures, where A is the proce- |#
#| dure defined above: |#
(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))
#| Give concise mathematical definitions for the functions com- |#
#| puted by the procedures f , g , and h for positive i |#

; fixed point algorithms.

(module fixed-point racket/base
  (provide fixed-point
           average-damping
           cubic-root)
  (define tolerance 0.0001)

  (define (average a b) (/ 2 (+ a b)))
  (define (square x) (* x x))

  (define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
      (< (abs (- v1 v2))
         tolerance))
    (define (try guess)
      (let [(next (f guess))]
        (if (close-enough? guess next)
          next
          (try next))))
    (try first-guess))


  ; average damping method
  (define (average-damping f)
    (lambda (x) (average x (f x))))

  ;to find sqrt
  (define (cube-root x)
    (fixed-point
      (average-damping
        (lambda (y) (/ x (square y)))
        0.1))))

