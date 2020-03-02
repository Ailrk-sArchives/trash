; from chapter 2.
; Data abstraction.

; implement pair with function alone.
#lang racket

(module ractinoals racket/base
  (require rationals-core)
  (provide make-rat
           numer
           denom
           add-rat
           sub-rat
           mul-rat
           div-rat)

  (module rationals-core racket/base
    (provide make-rat
             numer
             denom)
    (define (_gcd a b)
      (if (= b 0) a
        (_gcd b (remainder a b))))
    (define (make-rat n d)
      (cons n d))
    (define (numer x)
      (let ((g (_gcd (car x) (cdr x))))
        (/ (car x)) g))
    (define (denom x)
      (let ((g (_gcd (car x) (cdr x))))
        (/ (cdr x) g))))

  (define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x)))

  (define (add-rat x y)
    (make-rat (+ (* (numer x)
                    (denom y))
                 (* (numer y)
                    (denom x)))
              (* (denom x) (denom y))))

  (define (sub-rat x y)
    (make-rat (- (* (numer x)
                    (denom y))
                 (* (numer y)
                    (denom x)))
              (* (denom x) (denom y))))

  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))

  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y)))))

(module different-cons racket/base
  ; Ver 1.
  ; NOTE  As long as the upper layer of abstraction
  ; barrier can access all the interface the
  ; data structure provides, then it is a valid
  ; data abstraction.
  (provide cons-callback-choose
           car-callbak-choose
           cdr-callbak-choose
           cons-local-var
           car-local-var
           cdr-local-var
           cons-int
           car-int
           cdr-int)

  (define (cons-local-var x y)
    (define (dispatch m)
      (cond ((= m 0) x)
            ((= m 1) y)
            (else (error "Invalid argument"))))
    dispatch)

  (define (car-local-var z) (z 0))
  (define (cdr-local-var z) (z 1))


  (define pair1 (cons-local-var 1 2))
  (displayln (car-local-var pair1))


  ; Ver 2.
  ; NOTE Support all procudures for cons
  ; thus the implementation is an acceptable
  ; cons.
  (define (cons-callback-choose x y)
    (lambda (m) (m x y)))

  (define (car-callbak-choose z)
    (z (lambda (p q) p)))

  (define (cdr-callbak-choose z)
    (z (lambda (p q) q)))

  (define pair2 (cons-callback-choose 1 2))
  (displayln (car-callbak-choose pair2))


  ; E2.5
  ; represent int pair with only numbers and
  ; arithmetic operations if we represent pari
  ; a b as procuduct 2^a3^b

  (define (cons-int a b)
    (* (expt 2 a) (expt 3 b)))

  (define (div3 x) (= (remainder x 3) 0))
  (define div2 even?)
  (define (exclusive? z)
    (not (and (div3 z)
              (div2 z))))

  (define (car-int z)
    (if (exclusive? z) (log z 2)
      (car-int (/ z 3))))

  (define (cdr-int z)
    (if (exclusive? z) (log z 3)
      (cdr-int (/ z 2))))

  (define pair3 (cons-int 8 9))
  (displayln (car-int pair3)))

(module church-numeral racket/base
  ; church numerals to encode non negative numbers.
  ; Alonzon Chruch.
  (define (add-1 n)
    (lambda (f)
      (lambda (x)
        (f ((n f) x)))))
  (define zero (lambda (f) (lambda (x) x)))
  (define one (lambda (f) (lambda (x) (f x))))
  (define two (lambda (f) (lambda (x) (f (f x) x)))))

(module interval-arithmetic racket/base
  (define (make-interval lower upper)
    (cons lower upper))

  (define (lower-bound x) (max (car x) (cdr x)))
  (define (upper-bound x) (max (cdr x) (cdr x)))

  (define (add-interval x y)
    (make-interval (+ (lower-bound x)
                      (lower-bound y))
                   (+ (upper-bound x)
                      (upper-bound y))))

  (define (sub-interval x y)
    (make-interval (- (lower-bound x)
                      (upper-bound y))
                   (- (upper-bound x)
                      (lower-bound y))))

  (define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y))))

      (make-interval (min p1 p2 p3 p4)
                     (max p1 p2 p3 p4))))

  (define (div-interval x y)
    (mul-interval
      x
      (if (and (> (upper-bound y) 0)
               (< (lower-bound y) 0))
        (error "divide interval that across zero")
        (make-interval (/ 1.0 (upper-bound y))
                       (/ 1.0 (lower-bound y))))))
  )


