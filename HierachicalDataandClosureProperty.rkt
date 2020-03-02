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

  ; use parameter res as a stack.
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

  (define (square-tree tree)
    (map (lambda (sub)
           (if (pair? sub)
             (square-tree sub)
             (* sub sub)))
         tree))

  ; some abstraction here
  (define (tree-map proc tree)
    (map (lambda (sub)
           (if (pair? sub)
             (tree-map proc sub)
             (proc sub)))
         tree))

  ; use let with recursion. Very concise.
  ; logic here is split the powerset into
  ; power set without the first element and the
  ; power set with the first element.
  (define (powerset s)
    (if (null? s)
      (list '())
      (let ((rest- (powerset (cdr s))))
        (append rest-
                (map (lambda (x)
                       (cons (car s) x))
                     rest-)))))
  )

(module sequence-interface racket
  (define (acc op ini seq)
    (if (null? seq)
      ini
      (op (car seq)
          (acc op ini (cdr seq)))))

  (define (sieve pred seq)
    (cond ((null? seq) '())
          ((pred (car seq))
           (cons (car seq)
                 (sieve pred (cdr seq))))
          (else (sieve pred (cdr seq)))))


  ; an optimization algorithm for eval polynomial.
  ; it is proved that evaluation of any polynomial
  ; take at least the same amount of additions and
  ; multiplication as horner-eval do.
  (define (horner-eval x coeff-seq)
    (acc
      (lambda (coeff higher-term)
        (+ coeff (* x higher-term)))
      0
      coeff-seq))
  )

(module matrices racket
  (define (vec . z) (apply list z))

  ; map used as zip.
  (define (acc-n op ini seqs)
    (if (null? (car seqs))
      '()
      (cons (foldr op ini
                   (map car seqs))
            (acc-n op ini
                   (map cdr seqs)))))

  (define (dot-product v w)
    (foldr + 0 (map * v w)))

  (define (matrix-*-vector m v)
    (map (lambda (x)
           (dot-product v x))
         m))

  (define (transpose mat)
    (acc-n cons '() mat))

  (define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
      (map (lambda (v)
             (matrix-*-vector cols v))
           m)))

  ; use an extra parameter as stack
  ; very common technic when you want to
  ; reverse the order of enumerate over lists.
  (define (foldleft op ini seq)
    (define (iter res rest-)
      (if (null? rest-)
        res
        (iter (op res (car rest-))
              (cdr rest-))))
    (iter ini seq))
  )

(module pair-sum-prime racket

  (define (xrange low high)
    (if (>= low high)
      '()
      (cons low (xrange (+ 1 low) high))))

  (define (prime? x)
    (define (prime-iter x v)
      (cond ((or (zero? (remainder x v)) (< x 2))
             #f)
            ((< x (* v v))
             #t)
            (else
              (prime-iter x (+ 1 v)))))
    (prime-iter x 2))

  (define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))

  ; nested sequence.
  ; fold the result of map into a empty list.
  (define (flatmap proc seq)
    (foldr append '() (map proc seq)))

  (define (make-pair-sum pair)
    (list (car pair) (cadr pair)
          (+ (car pair)
             (cadr pair))))

  ; nested map with flat map.
  (define (prime-sum-pairs n)
    (map make-pair-sum
         (filter prime-sum?
                 (flatmap
                   (lambda (i)
                     (map (lambda (j)
                            (list i j))
                          (xrange 1 (- i 1))))
                   (xrange 1 n)))))

  ; deeply nested sequence.
  (define (permutations s)
    (if (null? s) (list '())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))
  )
