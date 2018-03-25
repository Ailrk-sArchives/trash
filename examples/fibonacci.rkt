#lang racket

(define (fibonacci-recursive n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibonacci-recursive (- n 1)) (fibonacci-recursive (- n 2))))))

(define (fibonacci-iterative n)
  (define (fib-iter a b counter)
    (if (= counter 0)
      b
      (fib-iter (+ a b) a (- counter 1))))
  (fib-iter 1 0 n))

(display (fibonacci-recursive 10))
(display "\n")
(display (fibonacci-iterative 10))
