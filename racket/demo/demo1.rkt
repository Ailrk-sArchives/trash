#lang racket
(require typed/racket)

#| block
   commane
|#

999999  ; intergers
#b111   ; binary => 7
#o111   ; octal => 73
#x111   ; hex => 273
3.14    ; reals
6.0e+23
1/2   ; rationals
1+2i  ; complex

'(+ 1 2) ; a literal list
(+ 1 1)
(quotient 5 2) ; => 2
(remainder 5 2) ; => 1
(exact->inexact 1/3) ; => 0.3333
(+ 1+2i 2-3i) ; => 3-1I

#t
#f
(not #t)
; short circuit.
(and 0 #f (error "doesn't get here"))
(or #f 0 (error "doesn't get here"))


;; characters
#\λ
#\u30BB

;; String
"Hello world!"

(string-append "append" " " "me")
(string-ref "Apple" 3)
(format "~a can be ~a" "strings" "formatted")
(printf "Racket is cool!\n")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(printf "Variable: \n\n")
(define some-var 10)
some-var

(define ⊆ subset?)
(⊆ (set 3 4) (set 4 2 3))

(printf "Local Binding: \n\n")
; local bind
(let ([me "Bob"])
  "Alice"
  me)

; bind for later bindings
(let* ([x 1]
       [y (+ x 1)])
      (* x y))

; define recursive and mutually recursive funtions.
(letrec ([is-even? (λ (n)
                      (or (zero? n)
                          (is-odd? (sub1 n))))]
         [is-odd? (λ (n)
                     (and (not (zero? n))
                          (is-even? (sub1 n))))])
  (is-even? 12))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Struct and Collections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; structs
; structs are by default immutable
(printf "Struct: \n\n")

(struct dog
  ([name : String]
   [breed : String]
   [age : Integer]))
(define joge
  (dog "joge" "corgi" 5))
joge
; data accessor
(dog? joge)
(dog-name joge)

; define mutable struct
(struct rgba-color
  ([r : Integer]
   [g : Integer]
   [b : Integer]
   [a : Real]) #:mutable)
(define burgundy
  (rgba-color 144 0 32 1.0))
(set-rgba-color-g! burgundy 10)
(rgba-color-g burgundy)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pairs (immutable)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(printf "Pairs: \n\n")
(cons 1 2)
(car (cons 1 2))
(cdr (cons 1 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List are linked list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(printf "Lists: \n\n")
(cons 1 (cons 2 (cons 3 null)))

; convienience data constructor for list
(list 1 2 3)

; quiasiquote: use backtick to eval function
(printf "Quiasiquote: \n\n")
`(1 ,(+ 1 1) 3)

(car '(1 2 3))
(cdr '(1 2 3))
(cadr (list 1 2 3))
(car (cdr (list 1 2 3)))
(cddr (list 1 2 3 4))
(cdr (cdr(list 1 2 3 4)))
(cadddr (list 1 2 3 4 5 6))

(append '(1 2) '(3 4))  ; like extend in python

(map add1 '(1 2 3))
(map + '(1 2 3) '(10 20 30))
(filter even? '(1 2 3 4))
(count even? '(1 2 3 4))
(take '(1 2 3 4) 2)
(drop '(1 2 3 4) 2)

;; Vectors are fixed-length arrays
(printf "Vectors\n\n")
#(1 2 3)
(vector-append #(1 2 3) #(4 5 6))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(printf "Sets\n\n")
(list->set '(1 2 3 4 5 6 7 1 2 3 4))

(set-add (set 1 2 3) 4)
(set-remove (set 1 2 3) 1)
(set-member? (set 1 2 3) 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hashes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(printf "Hashtable\n\n")
; create a immutable hashtable
(define m (hash 'a 1 'b 2 'c 3))
(hash-ref m 'a)

; extend hash table
(define m2 (hash-set m 'd 4))

(hash-remove m 'a)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(printf "Fucntions\n\n")
(lambda () "hello")
(define world (lambda () "world"))

(: hello-world (-> String))
(define hello-world
  (lambda () (string-append "hello" " " (world))))
(hello-world)

; multi-variadic functions with case-lambda
(printf "multi-variadic functions: \n\n")
(define hello
  (case-lambda
    [() "Hello world"]
    [(name) (string-append "Hello" " " name)]))

(hello)
(hello "Jimmy")

; optional arg
(define (hi [name "World"])
  (string-append "Hi" " " name))

(hi)

; pack args up in a list
(define (count-args . args)
  (format "You passed ~a args: ~a" (length args) args))
(count-args 1 2 3)

; desugared form:
(define count-args2
  (lambda args
    (format "You passed ~a args: ~a" (length args) args)))
(count-args2 1 2 3)

; With keywords
(define (hi-k #:name [name "World"]
              #:greeting [g "Hi"]
              . args)
  (format "~a ~a ~a extra args" g name (length args)))
(hi-k 1 2 #:greeting "Hi" #:name "Jimmy" 3 9)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Equality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(printf "Equality\n\n")
; number equality
(= 3 3.0)
(= 2 1)

; #t if two args refer to same obj in memory
(eq? '() '())
(let ([x '()]
      [y '()])
  (eq? x y))

(let ([x (list 3)]
      [y (list 3)])
  (eq? x y))

(let* ([x (list 3)]
       [y x])
  (eq? x y))

; `eqv?` compare num and character types
; for other types `eqv?` and `eq?` act the same.
(eqv? 3 3.0)
(eqv? (expt 2 100) (expt 2 100))
(eqv? (string-append "foo" "bar") (string-append "foo" "bar"))

; `equal?` for string bytestrings, pairs, mutable pairs, vectors, boxes.
(equal? 3 3.0)
(equal? (string-append "foo" "bar") (string-append "foo" "bar"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Control flow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(printf "Control Flow: \n\n")
(if #t
  "True"
  "False")
(if (member 'Groucho '(Harpo Groucho Zeppo))
  'yep
  'nope)

; cond chains a series of tests
(cond [(> 2 2) (error "wrong")]
      [(< 2 2) (error "wrong again")]
      [else 'ok])

; pattern matching

(define (fizzbuzz? n)
  (match (list (remainder n 3) (remainder n 5))
    [(list 0 0) 'fizzbuzz]
    [(list 0 _) 'fizz]
    [(list _ 0) 'buzz]
    [_ #f]))
(fizzbuzz? 15)

; looping with tail recursion
(define (loop i)
  (when (< i 10)
    (printf "i=~a\n" i)
    (loop (add1 i))))
(loop 6)

(printf "\n")
; for form
(for ([i 2])
  (printf "i=~a\n" i))

(printf "\n")

(for ([i (in-range 5 10)])
  (printf "i=~a\n" i))

; iteration over different sequences
(for ([i (in-list '(l i s t))])
  (displayln i))
(for ([i (in-vector #(v e c t o r))])
  (displayln i))
(for ([i (in-string "string")])
  (displayln i))
(for ([i (in-set (set 'x 'y 'z))])
  (displayln i))
(for ([(k v) (in-hash (hash 'x 1 'y 2 'z 3))])
  (printf "key: ~a, value: ~a\n" k v))

(for ([i 1000]
      #:when (> i 5)
      #:unless (odd? i)
      #:break (> i 10))
  (printf "i=~a\n" i))

; list comprehensions
(for/list ([i '(1 2 3)]) (add1 i))
(for/list ([i '(1 2 3)] #:when (even? i)) i)
(for/list ([i 10] [j '(x y z)]) (list i j))
(for/hash ([i '(1 2 3)]) (values i (number->string i)))
(for/sum ([i (in-range 1 101)]) i)
(for/and ([i 10] [j (in-range 10 20)]) (< i j)) ; all
(for/fold ([sum 0]) ([i '(1 2 3 4)]) (- sum i)) ; foldl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exceptions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(printf "Control Flow: \n\n")

(with-handlers ([exn:fail? (lambda (exn) "bad")]) (+ 1 "2"))
(with-handlers ([exn:break? (lambda (exn) " no time")])
               (sleep 0.1)
               "phew")
(with-handlers ([number? identity]) (+ 1 (raise 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mutation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(printf "Mutation \n\n")
(define n 5)
(set! n (add1 n))  ; assign new val to existing value
n

; box for explicit mutable values (like a reference)
(define n* (box 5))
(set-box! n* (add1 (unbox n*)))
(unbox n*)

; create mutable vector
(define vec (vector 2 2 3 4))
(define wall (make-vector 10 'bottle-of-beer))
(vector-set! vec 0 1)  ; update a vecotr slot
(vector-set! wall 8 'down)
vec
wall

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modules.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module cake racket/base
  (provide print-cake)
  (define (print-cake n)
    (show "    ~a     " n #\.)
    (show "  .-~a-.   " n #\|)
    (show " |  ~a  |  " n #\space)
    (show " ---~a---  " n #\-))

    (define (show fmt n ch)
      (printf fmt (make-string n ch))
      (newline)))

(require 'cake)
(print-cake 10)

; create mutable hashtable
(define m3 (make-hash))
(hash-set! m3 'a 1)
(hash-ref m3 'c 3)
(hash-remove! m3 'a)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Classes and Objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; create a class fish% (-% is idiomatic for class binding)
(define fish%
  (class object%
    (init size) ; init arg
    (super-new) ; superclass init
    (define current-size size)
    (define/public (get-size) current-size)
    (define/public (grow amt)
      (set! current-size (+ amt current-size)))
    (define/public (eat other-fish)
      (grow (send other-fish get-size)))))

(define charlie
  (new fish% [size 10]))

(printf "before ~a\n" (send charlie get-size))
(send charlie grow 6)
(printf "after ~a\n" (send charlie get-size))

; mixins
(define (add-color c%)
  (class c%
    (init color)
    (super-new)
    (define my-color color)
    (define/public (get-color) my-color)))
(define colored-fish% (add-color fish%))
(define charlie2 (new colored-fish% [size 10] [color 'red]))

(send charlie2 get-color)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; add a while loop
(define-syntax-rule (while condition body ...)
  (let loop ()
    (when condition
      body ...
      (loop))))

(let ([i 10])
  (while (< i 10)
         (displayln i)
         (set! i (add1 i))))

; hygienic macros. You cannot clobber existing variables.
(define-syntax-rule (swap! x y) ; ! is idiomatic for mutation
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))

(define tmp 2)
(define other 3)
(swap! tmp other)
(printf "tmp = ~a; other = ~a\n" tmp other)
; variable tmp is renamed to tmp_1 to avoid name
; conflict of tmp within the macros.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Contracts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; contracts impose constraints on values exported from modules

(module bank-account racket
  (provide (contract-out
             [deposit (-> positive? any)] ; always positive
             [balance (-> positive?)]))
  (define amount 0)
  (define (deposit a) (set! amount (+ amount a)))
  (define (balance) amount))
(require 'bank-account)
(deposit 5)
(balance)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input and output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; port, similar to file descriptor
(system "rm /tmp/tmp.txt")
(define out-port (open-output-file "/tmp/tmp.txt"))
(displayln "Racket is very dope" out-port)
(close-output-port out-port)

; append
(set! out-port (open-output-file "/tmp/tmp.txt" #:exists 'append))
(displayln "Hola mundo" out-port)
(close-output-port out-port)

; read
(define in-port (open-input-file "/tmp/tmp.txt"))
(displayln (read-line in-port))
(close-input-port in-port)

; context manager
(call-with-output-file "/tmp/tmp.txt"
                       #:exists 'update
                       (lambda (out)
                         (displayln "WOrld HeLlo" out)))

(call-with-input-file "/tmp/tmp.txt"
                      (lambda (in)
                        (displayln (read-line in))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typed racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct pt
  ([x : Real]
   [y : Real]))

(: distance (-> pt pt Real))
(define (distance p1 p2)
  (sqrt (+ (sqrt (- (pt-x p2) (pt-x p1)))
           (sqrt (- (pt-x p2) (pt-x p1))))))


