#lang racket

(define (factorial-recursive n)
  (if (= n 1)
    1
    (* n (factorial-recursive (- n 1)))))

(display (factorial-recursive 6))
(display "\n")

(define (factorial-iterative n)
  (define (factorial-iter product counter)
    (if (> counter n)
      product
      (factorial-iter (* counter product) (+ counter 1))))
  (factorial-iter 1 1))

(display (factorial-iterative 6))
(display "\n")
