#lang racket/base

; closure property:
;   The results of combining things with a
;   operation can be combined with that
;   operation.
;   // come from abastract algera.
;   // pair is close in cons

(module lists racket/base
  (define (_list-ref items n)
    (if (= n 0)
      (car items)
      (_list-ref (cdr items) (- n 1))))

  (define (_len items)
    (if (null? items)
      0
      (+ 1 (_len (cdr items)))))

  (define (_append list1 list2)
    (if (null? list1)
      list2
      (cons
        (car list1)
        (_append (cdr list1) list2))))

  (define (_last-pair items)
    (if (null? (cdr items))
      items
      (_last-pair (cdr items))))

  (define (_reverse items)
    (if (null? items)
      items
      (cons (car (_last-pair items)) (cdr items))))

  (define (same-parity . z)
    (define parity?
      (if (even? (car z)) even? odd?))
    (define (parity-cons l)
      (cond ((null? l) '())
            ((parity? (car l))
             (cons (car l)
                   (parity-cons (cdr l))))
            (else (parity-cons (cdr l)))))
    (parity-cons z))

  (define (_map proc items)
    (if (null? items) '()
      (cons (proc (car items))
            (_map proc (cdr items)))))

  (define (scale-list items factor)
    (_map (lambda (x) (* x factor)) items))


  (define (square-list items)
    (_map (lambda (x) (* x x)) items))

  (define (_for-each proc items)
    (if (null? (_map proc items)) #f #t))

  (define (count-leaves x)
    (cond ((null? x) 0)
          ((not (pair? x)) 1)
          (else (+ (count-leaves (car x))
                   (count-leaves (cdr x))))))
  )


