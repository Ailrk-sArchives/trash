#lang racket
(require racket/block)

;;;; Pairs
;; pairs are the same as cons cell in common lisp.

(define (pair-identity pair) (equal? (car pair) (cdr pair)))

(let ((xs (list (cons 23 23)
                (cons 22 23)
                (cons "see" "saw"))))
  (if (pair-identity (cons (car (cons 23 23)) (cdr (cons 34 23))))
      (block
       (print (car xs))
       (print "... ")
       (append (list 12 2 23) (cdr (car xs))))
      "hihi"))

;;;; list is the samse as lisp too
(let ((xs (list 34 'foo "bar"))
      (empty-xs '()))
  (block
   (displayln (length empty-xs))
   (displayln null)
   (displayln (list? '()))
   ; you have an array of handy combinators.
   (displayln (map (lambda (i) (+ i 10)) '(1 2 3 4)))
   (displayln (foldl (lambda (b a) (+ a b)) 0 '(1 2 3 4)))
   (displayln (filter (lambda (i) (> i 10)) '(1 11 1 11)))
   ; for each is traverse_
   (displayln (for-each (lambda (i) (display i)) '(1 2 3))))
  (displayln (member "Key" '("asd" "Key")))
  (display (assoc 'where '((where "asd") (when "pp") (who "Me")))))

;;;; struct
(define (add-bigger-fish lst)
  (struct fish (size) #:transparent)
  (cond
    ((null? lst) (list (fish 1)))
    (else (cons (fish (* 2 (fish-size (car lst)))) lst))))

(add-bigger-fish null)
;; this doesnt work because each time struct expression is evaluated a new type
;; is created. Inner add-bigger-fish doesn't satisfy fish?
(add-bigger-fish (add-bigger-fish null))

(struct fishuu (size) #:transparent)
(define (add-bigger-fishuu lst)
  (cond
    ((null? lst) (list (fishuu 1)))
    (else (cons (fishuu (* 2 (fishuu-size (car lst)))) lst))))
;; this works.
(add-bigger-fishuu null)
(add-bigger-fishuu (add-bigger-fishuu null))

(struct cake (candles)
  #:methods gen:custom-write    ;; custom write is a show instance.
  ((define (write-proc cake port mode)
     (define n (cake-candles cake))
     (show "   ~a   ~n" n #\. port)
     (show " .-~a-. ~n" n #\| port)
     (show " | ~a | ~n" n #\space port)
     (show "---~a---~n" n #\- port))
   (define (show fmt n ch port)   ;; define helper function
     (fprintf port fmt (make-string n ch)))))

(display (cake 4))
(display (cake 23))
