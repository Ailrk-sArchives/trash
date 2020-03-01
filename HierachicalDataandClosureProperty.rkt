#lang racket

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
    (define (rev-stack res items)
      (if (null? items) res
        (rev-stack (cons (car items) res)
                   (cdr items))))
    (rev-stack '() items))

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

  (define (deep-reverse items)
    (if (pair? items)
      (_reverse (map deep-reverse items)) items))

  (define (fringe tree)
    (cond ((null? tree) '())
          ((not (pair? tree)) (list tree))
          (else (append
                  (fringe (car tree))
                  (fringe (cdr tree))))))

  ; for nested tree structure
  ; there are base case null and
  ; flatened case where we reached the elements
  ; stored in tree.
  (define (scale-tree-nomap tree factor)
    (cond ((null? tree) '())
          ((not (pair? tree) (* factor tree)))
          (else
            (cons
              (scale-tree-nomap (car tree) factor)
              (scale-tree-nomap (cdr tree) factor))
            )))

  ; tree is a list too, so map can be a
  ; very powerful auxiliary for help.
  (define (scale-tree tree factor)
    (map (lambda (sub)
           (if (pair? sub)
             (scale-tree sub factor)
             (* sub factor)))
         tree))

  (define (square-tree tree factor)
    (map (lambda (sub)
           (if (pair? sub)
             (square-tree sub factor)
             (* sub factor)))
         tree))


  )


