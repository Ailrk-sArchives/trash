#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Chapter 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; c 1.1 Building abstraction with procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; 1.1.1 Elements of programming
;;;; 1.1.2 Naming and the Env
;; EX 1.2 expression practies
; visualize expressions as a tree
(/ (+ 5 4 
      (- 2 
         (- 3
            (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;;;; 1.1.3 Evaluating Combinations
;;;; 1.1.4 Compound procedures

;;;; 1.1.5 Substituion model for procedure application (eval)
;; EX 1.5 applicative-order evaluation and 
; normal-order evaluation test
;
; if it applicative, the func will check the first
; parameter x fist as soon as it be called, thus it
; will return 0 directly.
;
; But because racket is normal order eval in this
; case, it will evaluate both parameters before
; run the function body. and since p is a recursive
; definition, it will never end.
(define (p) (p))
(define (test x y)
  (if (= x 0)
    0
    y))
(test 0 (p))

;;;; 1.1.6 Combinational expressions and predicates

;;;; 1.1.7 Example of Square roots By Newtons method
(define (average x y)
  (/ (+ x y) 2))
(define (improve guess x)
  (average guess (/ x guess)))
(define (good-enough? guess x)
  (< (abs (- (* guess guess) x)) 0.00000001))
; here is a idiom for create iteration
; you have a (iter) and a (starter) 
;
; it is a iteration constructed by using only
; behaviour of procedure itself.
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))
(define (sqroot1 x)
  (sqrt-iter (/ x 2) x))
;
; Note: it is a basic abstraction.
; Note: decompose a bigger procedure down to smaller
;   one.
; Note: the purpose is to concern less.

;;;; 1.1.8 Procedures as Black-Box Abstraction
; with local definition
;
; Note: formal parameter define a bound variable
;   which when a procedure is called, a value is
;   bined to it. 
;
; Note: free variables are vars in env
(define (sqroot2 x)
  (define (avg a b)
    (/ (+ a b) 2))
  (define (good-enough? guess)
    (< (abs (- (* guess guess) x)) 0.0000001))
  (define (improve guess)
    (avg guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
  (sqrt-iter (/ x 2) x))
; this nesting of definition is called block sturct
; Note: procedure define here omit x since it is 
;   a free variable for all subprocedure under the
;   main scope



;;;;;;1.1 Conlusion;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;- Basic means of programming languge
;   1. primitive expressions
;   2. combination of premitive expressions
;   3. abstraction, give combination a name and manipulate them
;
;- Abstraction
;   Seal all uncessnary details and only provides thoes that matters.
;
;- procedure and data really aren't that difference. 
;   
;- Syntax
;   Syntaxs are basically special expressions for specific purpose.
;   The less the syntax is, the more flexible the program can be.
;   Lisp has almost no syntax, so you can combine expression in 
;   a way that not allowed in other languages.
;
;- Substitution Model
;   substitute abstraction with primitive expressions and combinations
;   so that they can be evaluated.
;   The basic operation suppored by a language is limited, all those
;   complex functionality are long combinations we give an abstraction
;   to.
;
;- Applicative order evaluation and Normal order evaluation
;   Applicative eval eval the most recent primitive expression emerged.
;   Normal order eval expand all expression then eval
;   The differences in order can cause different evaluation results.
;
;- Internal definition and block structure are means of abstraction.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; c 1.2 Procedures and process they generate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Note: procedure - a local evolution of 
;   computational process

;;;;1.2.1 Linear Recursion and Iteration
;; A Linear Recursion For Factorial
;
; it first recursively built up a chain of deferred
; operation, and recursively reduce in evaluation.
(define (factorial-rec n)
  (if (= n 1)
    1
    (* n (factorial-rec (- n 1)))))
; call stack for linear recursion:
; (f n)
; (* n (* (- n 1) (* (- n 2) (* (- n 3) ... )))))

;; A Linear Iteration For Factorial
;
; Note: the procedure is recursive procedure,
;   but the process it generate is really a iteration
;   its state is completely captured by param.
;
; Notice counter and carrying product are passed
; as parameters.
;
; Iteration always need an entrance procedure,
; the reason is it need multiple parameter to
; remember the state of previous iteration, which
; make the procedure definition tideous.
; An entrance procedure can clear up the top level
; interface
(define (factorial-iter n)
  (define (fact-iter product i max-out)
    (if (> i max-out)
      product
  ; created another me! with a little modification.
      (fact-iter (* i product)
                 (+ i 1)
                 max-out)))
  (fact-iter 1 1 n))
;call stack
;(f 1 2 3)
;(f 2 4 3)
;(f 4 8 3)
;
; Note difference between recursive process and 
; recursive procedure:
;   Recursive procedure: procedure call itself
;   Recursive process: how process evolve.
;
; Both of examples above are recursive procedure,
; but only the first one is recursive process.

;; Ex 1.9
;
; Characteristic for recursive proc:
; the recursive calling is wrapped in another
; statement.
; For iterative proc:
; the recursive calling is standalone, no state
; other than parameter need to be memorized by
; interpreter.
(define (inc a) (+ a 1))
(define (dec a) (- a 1))
;  A recursive process 
(define (plus1 a b)
  (if (= a 0)
    b
    (inc (plus1 (dec a) b))))
;(plus1 3 2)
;(inc (plus1 2 2))
;(inc (inc (plus1 1 2)))
;(inc (inc (inc (plus1 0 2))))
;(inc (inc (3)))
;(inc 4)
;(5)
; An iterative process
(define (plus2 a b)
  (if (= a 0)
    b
    (plus2 (dec a) (inc b))))
;(plus2 3 2)
;(plus2 2 3)
;(plus2 1 4)
;(plus2 0 5)
;(5)

;; Ex 1.10 Ackermann's function
;
; computable function but not primitive recursive
; it growth faster than exponetial function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) 
                 (A x (- y 1))))))

;;;; 1.2.2 Tree recursion
;; Fibbonacci
(define (fib n) 
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
;
;           (fib 5)
;           /     \
;      (fib 4)     (fib 3)
;      /     \      .....
;   (fib 3) (fib 2)
;   .....   .....
;stack
;(fib 5)
;(+ (fib 4) (fib 3))
;(+ (+ (fib 3) ((fib 2))) (+ (fib 2) (fib 1)))
; ...
;
; fib iter
(define (fib2 n)
  (define (fib-iter2 result i n)
    (if (= n 0)
      result
      (fib-iter2 (+ result i) i (- n 1))))
  (fib-iter2 0 1 n))

;; Example counting change
(define (count-change amount)
  ; first-val return the value of the first coin
  (define (first-val coin-types)
    (cond ((= coin-types 1) 1)
          ((= coin-types 2) 5)
          ((= coin-types 3) 10)
          ((= coin-types 4) 25)
          ((= coin-types 5) 50)))
  (define (cc amount coin-types)
    ;when amount to change is 0, we define there is one way
    ;to make the change
    (cond ((= amount 0) 1)
          ; when you don't need to change or have no optional
          ; coins, there is no way to change
          ((or (< amount 0) (= coin-types 0)) 0)
          (else (+ (cc amount (- coin-types 1)) 
                   (cc (- amount (first-val coin-types))
                       coin-types)))))
  (cc amount 5))
; dope graph
;                 (-----100,5------)
;                 /                \
;    (----100,4----)              (----99,5----)
;    /             \              /            \
;  (--100,3--) (--99,4--)     (--99,4--)   (--98,5--)
;  ...          ...             ...         ...
;Note: most of time if you can desctruct a problem into smaller
;   alike subproblem, try recursion.

;; Ex1.11
;f(n) = n when n < 3,
;f(n) = f(n-1) + 2f(n-2) + 3f(n-3)
;
; recursive
(define (fn1-11-rec n)
  (cond ((< n 3) n)
        (else (+ (fn1-11-rec (- n 1))
                 (* 2 (fn1-11-rec (- n 2)))
                 (* 3 (fn1-11-rec (- n 3)))))))
; iterative
;           (--------------------f(n)--------------------)
;           /                     |                       \
;    (----f(n-1)----)         (2f(n-2))               (3f(n-3))
;    /       |       \         |  |  |
;(f(n-2)(2(f(n-3)))(3f(n-4))   |  |  |                  ...
;                             /   |   \
;                    (2f(n-3))(4f(n-4))(6f(n-5))
(define (fn-11-iter n)
  ;; just like for, you almost always have a counter
  (define (fn n1 n2 n3 i)
    (cond ((= i 0) n1)
          ; f(2) = f(0)+2f(1)+3f(2)
          ; f(3) = f(1)+2f(2)+3f(3)
          ; f(4) = f(2)+2f(3)+3f(4)...
          ; look at the parameter:
          ; f(2): 0 1 2
          ; f(3): 1 2 3...
          (else (fn n2 n3 (+ n3 (* 2 n2) (* 3 n1)) (- i 1)))
          ))
  (fn 0 1 2 n))

;;Ex 1.12 
; Pascal's Triangle
; (n,r) = (n-1, r-1) + (n-1, r)
(define (pascal-triangle n r)
  ; edge condition (n,0) = (n,r) = 1
  (cond (
        (else (+ (pascal-triangle (- n 1) (- r 1))
                 (pascal-triangle (- n 1) r)))))

;;;; 1.2.3 Orders of Growth
;; Ex 1.15 Sine approximation
; sin value can be approximaeted by
;   sinx ~= x | x->0
;   and sinr = 3sin(r/3) - 4(sin(r/3))^3 by trignomertic
(define (sine rad)
  (define (cube x) (* x x x))
  (define (p x) (- (* 3 x) (* 4 (cube x))))
  (define (sine-iter r)
    (if (not (> (abs r) 0.1))
      ; when r is small sin(r) ~= r
      r
      ; the operator become more complex now, but the
      ; underline principle is exactly the same as +
      ; in linear recursion example.
      ;
      ; When r is large, keep substitue the larger sin(3/r)
      ; into smaller sin(3/r)
      (p (sine-iter (/ r 3.0)))))
  (sine-iter rad))
; When call (sine 12.15), how many times p applied to x?
;   ans: (/ (/ (/ (/ (/ 12.15 3) 3) 3) 3) 3) 3) = 0.05
;       Thus, p need to apply for 5 times
; 
; Order of growth in space and steps?
;
;   ans: it will iterate until r/(3^n) <= 0.1, thus
;       3^n = r/0.1
;       n = log(3, r/0.1) = log(r/0.1) / log3
;         = (log(r) - log(0.1)) / log3
;         = logr/log3 - log(0.1)/log3
;       Time complexity = O(log(n))

;;;; 1.2.4 Exponential

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; c 1.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; c 1.4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; c 1.5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; c 1.6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; c 1.7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




