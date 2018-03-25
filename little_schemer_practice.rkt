;; practice for little schemer's questions
#lang racket


;; Test procedure
(define test
  (lambda (foo name)
    (display name)
    (display ":     ")
    (display foo)
    (display "\n")))


;; 1.TOYS

;; (atom? a)
(define atom?
  (lambda (a)
    (and (not (pair? a)) (not (null? a)))))
;; test ;;
(test (atom? 'a) "atom? a")
(test (atom? '(a b c)) "atom? (a b c)")
(test (atom? (quote())) "atom? ()")
(display "\n")



;; 2. DO IT DO IT DO IT AGAIN

;; (lat? l)
;; a lat is a list composed by atom
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))
;; test ;;
(test (lat? '(a b c)) "lat? (a b c)")
(test (lat? '(a '(b) c)) "lat? (a (b) c)")
(display "\n")



;; (member? a lat)
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat))
                (member? a (cdr lat)))))))
;; test ;;
(test (member? 'a '(c d a (c f w))) "member? a (c d a (c f w))")
(test (member? 'a (quote ())) "member? ()")
(display "\n")



;; 3. CONS THE MAGNIFICENT
;; (rember a lat)
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? a (car lat)) (cdr lat))
              (else (cons (car lat) (rember a (cdr lat)))))))))
;; test ;;
(test (rember 'a '(c e d a)) "rember a (c e d a)")
(test (rember 'a '(c h)) "rember a (c h)")
(display "\n")



;; (firsts l)
(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else (cons (car (car l)) (firsts (cdr l)))))))
;; test ;;
(test (firsts '((e r s) (r a t) (p o i))) "firsts ((e r s) (r a t) (p o i))")
(display "\n")



;; (insertR new old lat)
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (cons new lat))
      (else (cond
              ((eq? (car lat) old) (cons (car lat) (cons new (cdr lat))))
              (else (cons (car lat) (insertR new old (cdr lat)))))))))
;; test ;;
(test (insertR 'new 'old '(sheme is a old garbage)) "insertR new old (sheme is a old garbage)")
(display "\n")



;; (insertL new old lat)
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (cons new lat))
      (else (cond
              ;; the reason why after recursion finished the (null? lat) will not
              ;; run is that when the program recurse at the point of old, it
              ;; return all left atoms directly.
              ((eq? (car lat) old) (cons new lat))
              (else (cons (car lat) (insertL new old (cdr lat)))))))))
;; test ;;
(test (insertL 'new 'old '(sheme is a old garbage)) "insertL new old (sheme is a old garbage)")
(test (insertL 'new 'old '()) "insertL new old ()")
(display "\n")



;; (subst new old lat)
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) old) (cons new (cdr lat)))
              (else (cons (car lat) (subst new old (cdr lat)))))))))
;; test ;;
(test (subst 'new 'old '(sheme is a old garbage)) "subst new old (sheme is a old garbage)")
(display "\n")



;; (subst2 new o1 o2 lat)
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((or (eq? o1 (car lat)) (eq? o2 (car lat))) (cons new (cdr lat)))
              (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))))
;; test ;;
(test (subst2 'new 'old1 'old2 '(sheme old2 is a old1 garbage)) "subst2 new old (sheme is a old garbage)")
(display "\n")



(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      (else
        (cond
          ((eq? (car lat) a) (multirember a (cdr lat)))
          (else (cons (car lat) (multirember a (cdr lat)))))))))
;; test ;;
(test (multirember 'a '(c a e d a)) "multirember a (c a e d a)")
(display "\n")



;; (multiinsertR new old lat)
(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else 
        (cond
          ((eq? (car lat) old)
           (cons (car lat) (cons new (multiinsertR new old (cdr lat)))))
          (else 
            (cons (car lat) 
                  (multiinsertR new old (cdr lat)))))))))
;; test ;;
(test (multiinsertR 'a 'b '(c b e d b)) "multiinsertR a b (c b e d b)")
(display "\n")



;; (multiinsertL new old lat)
(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else 
        (cond
          ((eq? (car lat) old)
           (cons new (cons old(multiinsertL new old (cdr lat)))))
          (else (cons (car lat) 
                      (multiinsertL new old (cdr lat)))))))))
;; test ;;
(test (multiinsertL 'a 'b '(c b e d b)) "multiinsertL a b (c b e d b)")
(display "\n")



;; (add1 n)
(define add1
  (lambda (n)
    (+ n 1)))
;; (sub1 n)
(define sub1
  (lambda (n)
    (- n 1)))


;; 4. PLAY WITH NUMBERS

;; (plus a b)
(define plus
  (lambda (a b)
    (cond
      ((zero? a) b)
      (else
        (plus (sub1 a) (add1 b))))))
;; test ;;
(test (plus 4 5) "plus 4 5")
(display "\n")



;; (minus a b)
(define minus
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else
        (minus (sub1 a) (sub1 b))))))
;; test ;;
(test (minus 4 5) "plus 4 5")
(display "\n")



;; (addup tup)
(define addup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (plus (car tup) (addup (cdr tup)))))))
;; test ;;
(test (addup '(5 6 9 2 9 93 2)) "addup (5 6 9 2 9 93 2)")
(test (addup '(5 6)) "addup (5 6)")
(display "\n")



;; (multiply n m)
(define multiply
  (lambda (m n)
    (cond
      ((zero? m) 0)
      (else
        (plus n (multiply (sub1 m) n))))))
(test (multiply 5 6) "multiply (5 6)")
(display "\n")



;; (tup+ tup1 tup2)
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else 
        (cons (plus (car tup1) (car tup2))
              (tup+ (cdr tup1) (cdr tup2)))))))
;; test ;;
(test (tup+ '(7 9 3) '(2 5 6)) "tup+ (7 9 3) (2 5 6)")
(test (tup+ '(7 9 ) '(2 5 6)) "tup+ (7 9 ) (2 5 6)")
(display "\n")



;; (gt a b)
(define gt
  (lambda (a b)
    (cond
      ((zero? a) #f)
      ((zero? b) #t)
      (else (gt (sub1 a) (sub1 b))))))
;; test ;;
(test (gt 5 2) "gt 5 2")
(test (gt 2 5) "gt 2 5")
(test (gt 2 2) "gt 2 2")
(display "\n")



;; (lt a b)
(define lt
  (lambda (a b)
    (cond 
      ((zero? b) #f)
      ((zero? a) #t)
      (else (lt (sub1 a) (sub1 b))))))
;; test ;;
(test (lt 5 2) "lt 5 2")
(test (lt 2 5) "lt 2 5")
(test (lt 2 2) "lt 2 2")
(display "\n")



;; (equal-primitive a b)
(define equal-primitive
  (lambda (a b)
    (cond
      ((zero? a) (zero? b))
      ((zero? b) #f)
      (else
        (equal-primitive (sub1 a) (sub1 b))))))

;; (equal a b)
(define equal
  (lambda (a b)
    (cond
      ((gt a b) #f)
      ((lt a b) #f)
      (else #t))))
;; test ;;
(test (equal 4 4) "equal 4 4")
(test (equal 2 4) "equal 2 4")
(display "\n")



;; (pow a b)
(define pow
  (lambda (a b)
    ;; a^b
    (cond
      ((zero? b) 1)
      (else (multiply a (pow a (sub1 b)))))))
;; test ;;
(test (pow 1 2) "pow 2 2")
(test (pow 2 4) "pow 2 2")
(display "\n")



;; (divide a b)
;; It minus a with b until b < a
;; which means it divide a by b by keep minus a with b
;; and return the number of operation needs to aheive < a b
(define divide
  (lambda (a b)
    (cond
      ((lt a b) 0)
      (else (add1 (divide (minus a b) b))))))
;; test ;;
(test (divide 4 2) "divide 4 2")
(test (divide 9 9) "divide 3 9")
(display "\n")



;; (length lat)
(define len
  (lambda (lat)
    (cond
      ;; if here use ((eq? (cdr lat) (quote())))
      ;; the value return will be 1 lower.
      ;; because when it reach last atom, (cdr lat) will be ()
      ((null? lat)
       0)
      (else
        (add1 (len(cdr lat)))))))
;; test ;;
(test (len '(cigs and beef add some beer)) "length (cigs and beef add some beer)")
(display "\n")



;; (pick n lat)
(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else
        (pick (sub1 n) (cdr lat))))))
;; test ;;
(test (pick 4 '(cigs and beef add some beer)) "pick (cigs and beef add some beer)")
(display "\n")



;; (rempick n lat)
;; old version
(define rempick-old
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else
        (cons (car lat) (rempick-old (sub1 n) (cdr lat)))))))


;; (no-nums lat)
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else
        (cond
          ((number? (car lat))
           (no-nums (cdr lat)))
          (else
            (cons (car lat) (no-nums (cdr lat)))))))))
;; test ;;
(test (no-nums '(3 b 4 s)) "no-nums (3 b 4 s)")
(display "\n")



;; (all-nums lat)
(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else
        (cond
          ((number? (car lat))
           (cons (car lat) (all-nums (cdr lat))))
          (else (all-nums (cdr lat))))))))
;; test ;;
(test (all-nums '(3 b 4 s 5  y 5)) "all-nums (3 b 4 s)")
(display "\n")



;; (eqan? a b)
(define eqan?
  (lambda (a b)
    (cond
      ((and (number? a) (number? b))
       (equal a b))
      ((and (atom? a) (atom? b))
       (eq? a b))
      (else #f))))
;; test ;;
(test (eqan? 'a 'a) "eqan? a a")
(test (eqan? 'a 'b) "eqan? a b")
(test (eqan?  1 1) "eqan? 1 1")
(test (eqan?  1 2) "eqan? 1 2")
(test (eqan? 'a 1) "eqan? a 1")
(display "\n")



;; (occur a lat)
(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else
        (cond
          ((eqan? a (car lat))
           (add1 (occur a (cdr lat))))
          (else
            (occur a (cdr lat))))))))
;; test ;;
(test (occur 'a '(a s i a n)) "occur a (a s i a n)")
(display "\n")



;; (one-long? n)
;; the longer version
(define one-long?
  (lambda (n)
    (cond
      ((equal n 1)
       #t)
      (else
        #f))))


;; (one? n)
(define one?
  (lambda (n)
    (equal 1 n)))
;; test ;;
(test (one? 1) "one 5")
(test (one? 5) "one 5")
(display "\n")



;; (rempick n lat)
(define rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else
        (cons (car lat) (rempick (sub1 n) (cdr lat)))))))
;; test ;;
(test (rempick 6 '(c d e f g a b)) "rempick a (c d e f g a b)")
(display "\n")



;; (rember* a l)
(define rember*
  (lambda (a l)
    (cond
      ;; basic case. When the list is (), return ()
      ((null? l) (quote ()))
      ;; when current S-expr is an atom, start to check if it is a.
      ((atom? (car l))
       (cond
         ;; a == (car l) ??
         ((eq? a (car l))
          ;; cons this S-expr with the recursion result.
          ;; which is another list with a removed.
          (cons (car l) (rember* a (cdr l)))
          (else 
            ;; a != (car l), 
            ;; just cons this element with next (restore orgin list)
            (cons (car l) (cdr l))))))
    ;; if current is not an atom (so it is a list)
    (else 
      ;; recurse both car and l
      ((cons (rember* a (car l))
             (rember* a (cdr l)))))))


      )
;; test ;;
(test (rember* 'a '((a b c) d a ((c d)))) "rember* a ((a b c) d a ((c d))))")





























