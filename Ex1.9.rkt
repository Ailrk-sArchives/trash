;; Which of the following procedure is recursive? which is iterative?
#lang racket

(define (inc x)
  (+ x 1))
(define (dec x)
  (- x 1))


;; Ans
;; (add1) is recursive. It doesn't keep tracking the value of a,
;; instread, the number of (inc) decide the number b add with.
(define (add1 a b)
  (if (= a 0)
    b
    (inc (add1 (dec a) b))))
(display (add1 4 5)) (display "\n")


;; (add2) is iterative. For each turn, ((dec a) (inc b)) is passed
;; to the add2, the process state is tracked by parameter completely.
(define (add2 a b)
  (if (= a 0)
    b
    (add2 (dec a) (inc b))))
(display (add2 4 5)) (display "\n")

