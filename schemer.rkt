#lang racket/base
;; <Little_schemer_practice>.

; The First Commandment 
; ask null? as the first condition.

; atom? atom checker
(define atom?
  (lambda (x)
    (and 
      (not (null? x))
      (not (pair? x)))))

; lat? list atom checker
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

; member? check if atom is a member of a list
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (equal/s (car lat) a)
                (member? a (cdr lat)))))))

; The second Commandment
; use cons to build lists

; remove a member
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (cdr lat))
      (else
        (cons (car lat)
              (rember a (cdr lat)))))))

; get first s-expr of a list with () or non empty lists
; ((a b) (c d) (e f)) -> (a c e)
(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l))
                  (firsts (cdr l)))))))

; insert new value to the right of old value.
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? (car lat) old)
               (cons old
                     (cons new (cdr lat))))
              (else (cons (car lat)
                          (insertR new old (cdr lat)))))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? (car lat) old) (cons new lat))
              (else (cons (car lat)
                          (insertL new old (cdr lat)))))))))

; The Third  Commandment
; when building a list, describe the first typical element, 
; then cons it into the natural recursion.

; substitute the first occurance.
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? (car lat) old) (cons new (cdr lat)))
              (else (cons (car lat)
                          (subst new old (cdr lat)))))))))

; sub one of the give old value.
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      (else (cond 
              ((or (eq? o1 (car lat))
                   (eq? o2 (car lat)))
                   (cons new (cdr lat)))
              (else (cons (car lat)
                          (subst2 new o1 o2 (cdr lat)))))))))

; remove all occurance of a in lat
(define multirember
  (lambda (a lat)
    (cond 
      ((null? lat) '())
      (else (cond
              ((equal/s (car lat) a) (multirember a (cdr lat)))
              (else (cons (car lat)
                          (multirember a 
                                       (cdr lat)))))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) 
       (cons old 
             (cons new
                   (multiinsertR new old (cdr lat)))))
      (else
        (cons (car lat) (multiinsertR new old (cdr lat)))))))

; The fourth Commandment
; always change at least one argument while recuring. 

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) 
       (cons new 
             (cons old (multiinsertL new old (cdr lat)))))
      (else
        (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond 
      ((null? lat) '())
      ((eq? old (car lat))
       (cons new (multisubst new old (cdr lat))))
      (else
        (cons (car lat) (multisubst new old (cdr lat)))))))

(define add1 (lambda (n) (+ n 1)))
(define sub1 (lambda (n) (- n 1)))

(define plus 
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else
        (add1 (plus n (sub1 m)))))))

(define minus
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else
        (sub1 (minus n (sub1 m)))))))

; define tuple: list of number (1 2 3 4)
(define addup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else
        (plus (car tup)
              (addup (cdr tup)))))))

(define mul
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (plus n (mul n (sub1 m)))))))

(define tup+
  (lambda (t1 t2)
    (cond 
      ((null? t1) t2)
      ((null? t2) t1)
      (else 
        (cons 
          (plus (car t1) (car t2))
          (tup+ (cdr t1) (cdr t2)))))))

(define gt
  (lambda (n m)
    (cond
      ((zero? m) #t)
      ((zero? n) #f)
      ((zero? (minus m n)) #f)
      (else (gt (sub1 n) (sub1 m))))))

; equal for number comparison
(define eq_ (lambda (n m) (not (or (gt n m) (lt n m)))))

(define lt
  (lambda (n m)
    (cond
      ((zero? (minus n m)) #f)
      (else (not (gt n m))))))

(define exp_
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else
        (mul n (exp_ n (sub1 m)))))))


(define quotient_
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (quotient_ (minus n m) m))))))

(define length_
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else
        (add1 (length_ (cdr lat)))))))

(define pick_
  (lambda (n lat)
    (cond
      ((zero? n) (car lat))
      (else
        (pick_ (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? n) (cdr lat))
      (else
        (cons (car lat)
              (rempick (sub1 n) (cdr lat)))))))

; remove all numbers in a list
(define no-nums
  (lambda (lat)
    (cond 
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else
        (cons (car lat)
              (no-nums (cdr lat)))))))

; extract all the number from a tup
(define all-nums
  (lambda (lat)
    (cond 
      ((null? lat) '())
      ((not (number? (car lat))) (all-nums (cdr lat)))
      (else
        (cons (car lat)
              (all-nums (cdr lat)))))))

; check if two arguments are the same atoms.
(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (eq_ a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? a (car lat)) (add1 (occur a (cdr lat))))
      (else
        (occur a (cdr lat))))))

(define one?
  (lambda (n)
    (cond
      ((zero? n) #f)
      (else (zero? (sub1 n))))))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l)) 
       (cond 
         ((eq? (car l) a)
          (rember* a (cdr l)))
         (else (cons (car l)
                     (rember* a (cdr l))))))
       (else (cons (rember* a (car l))
                   (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond 
         ((eq? old (car l)) 
          (cons old 
                (cons new
                      (insertR* new old (cdr l)))))
         (else
           (cons (car l)
                 (insertR* new old (cdr l))))))
      (else
        (cons (insertR* new old (car l))
              (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? a (car l))
          (add1 (occur* a (cdr l))))
         (else
           (occur* a (cdr l)))))
      (else 
        (plus (occur* a (car l))
              (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond 
         ((eq? old (car l))
         (cons new (subst* new old (cdr l))))
        (else 
          (cons (car l) (subst* new old (cdr l))))))
      (else
        (cons (subst* new old (car l))
              (subst* new old (cdr l)))))))

(define insertL* 
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new (subst* new old (cdr l))))
         (else
           (cons (car l) (subst* new old (cdr l))))))
      (else (cons 
              (insertL* new old (car l))
              (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond 
      ((null? l) #f)
      ((atom? (car l))
       (or (eq? a (car l))
           (member* a (cdr l))))
      (else
        (or (member* a (car l))
            (member* a (cdr l)))))))

; it only go in one branch of the tree, so not a *-func.
(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

; ; ; ; ; refinement ; ; ; ; 
; check if two lists are identical.
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
        (and (equal/s (car l1) (car l2))
             (eqlist? (cdr l1) (cdr l2)))))))

; check if two S-expressions are equal
(define equal/s
  (lambda (s1 s2)
    (cond 
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))


; The Sixth Commandment
; Simplify only after the funcition is correct

(define rember/s*
  (lambda (s l)
    (cond
      ((null? l) '())
      ((atom? (car l))   
       (cond
        ((equal/s s (car l)) (rember/s* s (cdr l)))
        (else 
          (cons (car l)
                (rember/s* s (cdr l))))))
       (else
         (cons (rember/s* s (car l))
               (rember/s* s (cdr l)))))))


;; Shadows

; n + 3: a arithmetic expression
; (n + 3): a representation of the arithmetic expression.
;   it is a S-expr and can be feeded as argument.
(define or-list 
  (lambda (l)
    (cond ((null? l) #f) 
          (else (or (car l) 
                  (or-list (cdr l))))))) 

(define numbered? 
  (lambda (aexp) 
    (cond
      ((atom? aexp) (number? aexp))
      ((or-list (map (lambda (o) (eq? (car (cdr aexp)) o))
                '(plus minus mul)))
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp))))))
      (else #f))))


;; The Seventh Commandment
;; Recur on the subpart that are of the same nature.

;; The Eighth Commandement
;; Use help functions to abstract from representations.


(define value
  (lambda (nexp)
    (define 1st-sub-expr (lambda (aexp) (car (cdr aexp))))
    (define 2ed-sub-expr (lambda (aexp) (car (cdr (cdr aexp)))))
    (define operator (lambda (aexp) (car aexp)))
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) 'plus)
       (plus (value (1st-sub-expr nexp))
             (value (2ed-sub-expr nexp))))
      ((eq? (operator nexp) 'minus)
       (minus (value (1st-sub-expr nexp))
              (value (2ed-sub-expr nexp))))
      ((eq? (operator nexp) 'mul)
       (mul (value (1st-sub-expr nexp))
              (value (2ed-sub-expr nexp))))
      ((eq? (operator nexp) 'quotient_)
       (quotient_ (value (1st-sub-expr nexp))
              (value (2ed-sub-expr nexp))))
      (else
        (exp_ (value (1st-sub-expr nexp))
              (value (2ed-sub-expr nexp)))))))

;; new number representation: () -> 0 (()) -> 1 ...
(define sero?  (lambda (n) (null? n)))
(define edd1 (lambda (n) (cons '() n)))
(define zub1 (lambda (n) (cdr n)))
(define qlus 
  (lambda (m n)
    (cond
      ((sero? m) n)
      (else
        (edd1 (qlus n (zub1 m)))))))


;; Relations

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else
        (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond 
      ((null? lat) '())
      ((member? (car lat) (cdr lat))
       (makeset (cdr lat)))
      (else 
        (cons (car lat) (makeset
                          (multirember (car lat)
                                       (cdr lat))))))))

(define subset? 
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else
        (and (member? (car set1) set2)
             (subset? (cdr set1) set2))))))


(define eqset?
  (lambda (set1 set2) (and (subset? set1 set2) (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else
        (or (member? (car set1) set2)
            (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (s1 s2)
    ((lambda (set1 set2)
       (cond
         ((null? set1) '())
         ((member? (car set1) set2)
          (cons (car set1) (intersect (cdr set1) set2)))
         (else 
           (intersect (cdr set1) set2))))
    (makeset s1)
    (makeset s2))))

(define union
  (lambda (s1 s2)
    ((lambda (set1 set2)
      (cond
        ((null? set1) set2)
        ((member? (car set1) set2)
         (union (cdr set1) set2))
        (else
          (cons (car set1)
                (union (cdr set1) set2)))))
     (makeset s1)
     (makeset s2))))

(define difference
  (lambda (s1 s2)
    (define one-side-difference
      (lambda (set1 set2)
       (cond
         ((null? set1) '())
         ((member? (car set1) set2)
          (one-side-difference (cdr set1) set2))
         (else 
           (cons (car set1) 
                 (one-side-difference (cdr set1) set2))))))
    ((lambda (l1 l2)
      (union (one-side-difference l1 l2)
             (one-side-difference l2 l1)))
     (makeset s1)
     (makeset s2))))

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set) (intersectall (cdr l-set)))))))


; make representation
(define first (lambda (p) (car p)))
(define second (lambda (p) (car (cdr p))))
(define third (lambda (p) (car (cdr (cdr p)))))
(define build (lambda (s1 s2) (cons s1 (cons s2 '()))))
(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

; define a pair (S-expr S-expr)
(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

; define: fun stand for function 
; check if is finite func. f(2) == f(3) is not defined here.
(define fun? (lambda (rel) (set? (firsts rel))))

; reverse the domain and range.
(define reverl
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else
        (cons (revpair (car rel))
              (reverl (cdr rel)))))))

(define one-to-one? 
  (lambda (l)
    (and (fun? l)
         (fun? (reverl l)))))


(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) '())
      ((test? (car l) a) (cdr l))
      (else (cons (car l)
                  (rember-f test? a 
                            (cdr l)))))))

; lambda...

; currying
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))
; (define eq?-salad (eq?-c 'salad))

(define seqL (lambda (new old l) (cons new (cons old l))))
(define seqR (lambda (new old l) (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((eq? (car l) old)
         (seq new old (cdr l)))
        (else (cons (car l)
                    ((insert-g seq) new old (cdr l))))))))

;; The Ninth Commandment
;; Abstract common patterns with a new function.

;; a substitution with insert.
(define seqS (lambda (new old l) (cons new l)))
(define subst_ (insert-g seqS))  


(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x 'plus) plus)
      ((eq? x 'minus) minus)
      ((eq? x 'mul) mul)
      ((eq? x 'quotient_) quotient_)
      ((eq? x 'minus) minus)
      (else exp_))))

(define value_
  (lambda (nexp)
    (define 1st-sub-expr (lambda (aexp) (car (cdr aexp))))
    (define 2ed-sub-expr (lambda (aexp) (car (cdr (cdr aexp)))))
    (define operator (lambda (aexp) (car aexp)))
    (cond
      ((atom? nexp) nexp)
      (else
        ((atom-to-function (operator nexp))
         (value_ (1st-sub-expr nexp))
         (value_ (2ed-sub-expr nexp)))))))

(define eq-tuna (eq?-c 'tuna))

;; pass a partial func
(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat))
       (multiremberT test? (cdr lat)))
      (else
        (cons (car lat)
              (multiremberT test? (cdr lat)))))))


;; The Tenth Commandment
;; Build funtioncs to collect more than one value at a time.


;; collector/continuation.
; first call (multirember&co 'a '(b c a))
; in last layer, (multirember&co 'a '())
;
; replaced with value: 
; (multirember&co 'a '(b c a) col)
; (multirember&co 'a '(c a) (lambda col))
; (multirember&co 'a '(a) (lambda (lambda col)))
; (multirember&co 'a '() (lambda (lambda (lambda (col)))))
;   ((lambda (n s)                        ;last recur layer
;     ((lambda (n s)                      
;        ((lambda (n s)                   ;first recur layer
;           (col (cons (car lat) n) s))  ;;lat='(b c a) ;n: (c d), s: (a)
;         (cons (car lat) n) s))         ;;lat='(c a)   ;n: (c),   s: (a)
;      n (cons (car lat) s))) '() '())   ;;lat='(a)     ;n: (),    s: (a)

(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat) (col '() '()))
      ((eq? (car lat) a) 
       (multirember&co a 
         (cdr lat)
         (lambda (newlat seen) 
           (col newlat (cons (car lat) seen)))))
      (else
        (multirember&co a 
          (cdr lat) 
          (lambda (newlat seen)
            (col (cons (car lat) newlat) seen)))))))

;; examples of collector.
(define rm (lambda (x y) x))
(define contain? (lambda (x y) (null? y)))
(define count? (lambda (x y) (length y)))
; (multirember&co 'a '(b c) contain?)
; (multirember&co 'a '(b c a d e a) count?)
; (multirember&co 'a '(b c a d e a) rm)

(define multiinsertLR&co 
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col '() 0 0))  
      ((eq? (car lat) oldL)
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons new
                                      (cons oldL newlat))
                                (add1 L) R))))
      ((eq? (car lat) oldR)
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons oldR
                                      (cons new newlat))
                                L (add1 R)))))
      (else
        (multiinsertLR&co new oldL oldR (cdr lat)
                          (lambda (newlat L R)
                            (col (cons (car lat) newlat)
                                 L R)))))))

(define evens-only*
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((even? (car l))
          (cons (car l) (evens-only* (cdr l))))
         (else (evens-only* (cdr l)))))
      (else (cons (evens-only* (car l))
                  (evens-only* (cdr l)))))))

; 1. pass a list and a colletor func
; 2. base case of collector is ('() 1 0). 
; 3. stands for list of all even number, product of evens, sum of pluses.
; 4. if (car l) is atom and is even, recur with new lambda 
;    the lambda will cons (car l) into '(), and time (car l) with 1.(in base case)
; 5. if (car l) is atom but not even, just plus (car l) with 0 (in base case)
; 6. if (car l) is a list
;    it call itself with (car l) // a list as l, and create a huge collector.
;    the collector use evens-only*&co as col to vist (cdr l)
;    and there is another collector in this collector, we call it subcollector.
;    the subcollector will cons the result of (car l) with (cdr l).

(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l) (col '() 1 0))
      ((atom? (car l))
       (cond
         ((even? (car l))
          (evens-only*&co (cdr l)
                          (lambda (newl p s)
                            (col (cons (car l) newl)
                                 (* (car l) p) s))))
         (else 
           (evens-only*&co (cdr l)
                           (lambda (newl p s)
                             (col newl
                                  p (+ (car l) s)))))))
      (else 
        (evens-only*&co (car l)
                        (lambda (al ap as)
                          (evens-only*&co (cdr l)
                                          (lambda (dl dp ds)
                                            (col (cons al dl)
                                                 (* ap dp)
                                                 (+ as ds))))))))))
(define get-result
  (lambda (newl product sum)
    (cons
      (cons
        product
        (cons
          sum '())) newl)))
; (evens-only*&co '((10 9 1 2) 3 10 (9 9) 7 98) get-result)

;
; unnatrual recursion 
;
(define looking
  (lambda (a lat)
    (keep-looking a (pick_ 1 lat) lat)))

(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn)
       (keep-looking a (pick_ sorn lat) lat))
      (else (eq? sorn a)))))

; partial recusrive function and total recursive function
(define eternity
  (lambda (x)
    (eternity x)))

; take a pair with first component is a pair, it shift the (car (cdr ) first pair)
; into the second component.
(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

; each recursion change its parameters, and it cannot garantee it apporach the goal.
; it is a total recursive func, every para has a value.
(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (align (shift pora)))
      (else (build (first pora)
                   (align (second pora)))))))


; wrong. first argument gets simpiler but second get more complicated.
(define pora-length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
        (+ (pora-length* (first pora))
           (pora-length* (second pora)))))))

; pay 'twice' attention to first component, arguments get simpler. 
(define pora-weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
        (+ (* (pora-weight* (first pora) 2))
           (pora-weight* (second pora)))))))

; it is not a total recursive func.
(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (shuffle (revpair pora)))
      (else (build (first pora)
                   (shuffle (second pora)))))))

; not a total function.
; Lothar Collatz.
; It is only known that it is undefined when n=0
(define C
  (lambda (n)
    (cond
      ((one? n) 1)
      (else
        (cond
          ((even? n) (C (/ n 2)))
          (else (C (add1 (* 3 n)))))))))


; Ackermann  // unnatrual recursion, total function.
(define A
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n)
               (A n (sub1 m)))))))

; (A 4 3) take forever.

; HOW TO MAKE A Y COMBINATOR

;; Version 1 ;;
; (lambda (l)
;   (cond
;     ((null? l) 0)
;     (else 
;       (add1
;         ((lambda (l)
;            (cond 
;              ((null? l) 0)
;              (else (add1
;                      (eternity (cdr l))))))
;          (cdr l)))))) 

;; Version 2 ;;
; ((lambda (f)
;    (lambda (l)
;      (cond
;        ((null? l) 0)
;        (else (add1 (f (cdr l)))))))
;  ((lambda (g)
;     (lambda (l)
;       (cond
;         ((null? l) 0)
;         (else (add1 (g cdr l))))))
;   eternity))

;; Version 3 ;;
; ((lambda (mk-length)
;    (mk-length 
;      (mk-length eternity)))
;  (lambda (len)
;    (lambda (l)
;      (cond
;        ((null? l) 0)
;        (else (add1 (len (cdr l))))))))
; Using arguments to abstract, same as define.

; RECURSIVE DEFINITION.

; create a func, pass a lambda as argument to it
; 
;Version 4
; ((lambda (mk-length)
;    (mk-length mk-length))   ;engine 1
;  (lambda (mk-length)        ;recur 
;    (lambda (l)              ;primitive
;      (cond
;        ((null? l) 0)
;        (else (add1 
;                ((mk-length mk-length)  ;engine 2
;                 (cdr l))))))))

; Version 6 *
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (len)                       ; (lambda (x) f x)
      (lambda (l)                       ; also return a 
        (cond                           ; function with one
          ((null? l) 0)                 ; argument.
          (else                         ;
            (add1 (len (cdr l)))))))    ;
    (lambda (x)
      ((mk-length mk-length) x)))))

; version 7 move the comment part in version 6 out since 
; doesn't depends on mk-length at all.
((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
            ((mk-length mk-length) x))))))
 (lambda (len)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (len cdr l)))))))

; applicative-order Y combinator:
(define Y
  (lambda (g)
    ((lambda (f) (f f))
     (lambda (f) (g (lambda (x) ((f f) x)))))))

;
; Y combinator recursive nature prove. 
;
; Y := λf.(λx.(f (x x)) λx.(f (x x)))
; Y g = λf.(λx.(f (x x)) λx.(f (x x)))g
;     = λx.(g (x x)) λx.(g (x x))
;     = λy.(g (y y)) λx.(g (x x))
;     = g (λx.(g (x x)) λx.(g (x x)))
; 
; Meanwhile, 
; g(Y g) = g (λf.(λx.(f (x x)) λx.(f (x x)))g)
;        = g (λx.(g (x x)) λx.(g (x x))) 
;        = (Y g)
; Thus, (Y g) = (g (Y g)) = ... = (g(g ... (g (Y g) ...)))
; any of them works the same as Y.

;
; applicative Y combinator, the versoin we write.
;
; Y := λg.(λf.(f f) λf.g(λx.((f f) x)))
; Y h = λg.(λf.(f f) λf.g(λx.((f f) x)))h
;     = λf.h(λx.((f f) x)) λf.h(λx.((f f) x))
;     = h( λx.((λf.h(λx.((f f) x))  λf.h(λx.((f f) x))) x) ) 
;     let F = λx.((λf.h(λx.((f f) x))
; then
;     = h((F F) x)
;     = h(h ((F F) x)) = ...

; omega combinator: apply a func to itself.
; (lambda (o) (o o))

; apply to Y to achieve generic recursion.
; ((Y (lambda (len)
;      (lambda (l)
;        (cond
;          ((null? l) 0)
;          (else
;            (add1 (len (cdr l))))))))
;  '(1 2 3 9 9 9 9 9 9 9 9 9 9 9 9 9 9))
