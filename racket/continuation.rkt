#lang racket
; https://stackoverflow.com/questions/22888722/could-someone-explain-call-cc-in-simple-words

; say you want to compute (+ (* 2 3) (/ 10 2))
; you can't evaluate everything at the same time,
; so the actually implementation will be evaluing simple
; components by parts until the evaluation finished.

(+ (* 2 3) (/ 10 2))

(define (*& x y cont) (cont (* x y)))
(define (+& x y cont) (cont (+ x y)))
(define (-& x y cont) (cont (- x y)))
(define (/& x y cont) (cont (/ x y)))
(define (zero?& x y cont) (cont (zero? x y)))

; new write it in continuation style.
; you keep passing the rest continuation
(define expression
  (lambda (k)
    (*& 2 3
        (lambda (r1)
          (/& 10 2
              (lambda (r2)
                (+& r1 r2 k)))))))
(expression identity)

(define a 0)
; but what if the division is invalid? in case you are
; dividing a value by 0, you want to exit the evaluation
; all together. what should you do in that case?

; to handle zero case with cps only, we need to write this:
(define expression1
  (lambda (k)
    (*& 2 3
        (lambda (r1)
          (zero?& a
                  (lambda (azero?)
                    (if (azero?)
                      (k +inf.0)
                      (/& 10 a
                          (lambda (r2)
                            (+& r1 r2 k))))))))))

; call/cc helps you to simplify the code above
(call/cc
  (lambda (k)
           (+ (* 2 3)
              (/ 10 (if (zero? a) (k +inf.0) a)))))

; this will print hello 11 times
(let* ((c 10)
       (r (call/cc (lambda (exit) exit))))
  (display "Hello\n")
  (cond ((zero? c) 'done)
        (else (set! c (- c 1))
              (r r))))

(define (lstadd1 lst)
  (call/cc (lambda (exit)
             (let loop ((lst lst))
               (cond ((pair? lst) (cons (add1 (car lst)) (loop (cdr lst))))
                     ((null? lst) '())
                     (else (exit #f)))))))
