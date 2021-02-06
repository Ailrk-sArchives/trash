;; this is actually quite simple.
;; why ppl likes to make it seems so fancy...

;; this is the most primitive form of evaluator.
;; no error checking, no arity checking, nothing.

;; buts this is the core of any list. You can add more
;; features into the evaluators as you want.

;; it's also easy to create programming tools by injecting
;; related code into the evaluator.

(define (evaluate e env)
  (if (atom? e)
      ;;;; environment ;;;;
      ;; programs and there representations are separated.
      ;; like symbols are tangible textual object, but varaible is
      ;; just an imaginary object in our program.
      ;; to look up the value of a variable, we use the equivelent
      ;; symbols as key to search in the environment.
      (cond ((symbol? e) (lookup e env))
            ;;;; autoquotes ;;;;
            ;; It can be hard to determine what does it mean for some
            ;; objects be evaluated. For instance, what does a function
            ;; evaluates to?
            ;; So we only have these clearly defined autoquotes.
            ((or (number? e)
                 (string? e)
                 (char? e)
                 (boolean? e)
                 (vector? e)) e)
            (else (error "Can't evaluate e")))
      (case (car e)
        ;;;; quote (discriminator between data and program) ;;;;
        ;; because we can represent our lisp program in terms of it's value
        ;; (symbols and list), it's necessary to have a tag to tell you
        ;; whether an object is part of the code or it's just value.
        ((quote) (cadr e))                  ;; special forms
        ;;;; meta circular ;;;;
        ;; one thing to note is we are implemeting scheme with scheme,
        ;; so there are a lot of facility can be shared between these two
        ;; implementations.
        ;; For example, we can reuse boolean values from mit-scheme.
        ((if) (if (not (eq? (evaluate (cadr e) env) #f))
                  (evaluate (caddr e) env)
                  (evaluate (cadddr e) env)))
        ((begin) (eprogn (cdr e) env))
        ((set!) (update! (cadr e) (cddr e) env))
        ((lambda) (make-function (cadr e) (cddr e) env))  ;; abstraction
        (else (invoke (evaluate (car e) env) ;; function applicatoin
                      (evlis (cdr e) env))))))

;;;; sequence: putting series of evaluations in order ;;;;
;; in an algol like language it's like your begin ... end block.
;; what you expect is a group of forms be evaluated sequentially,
;; and there side effects are performed sequentially too.
;; or in anoter word, evaluate a list of expressions in order.
(define (eprogn e env)
  (if (pair? e)
      (if (pair? (cdr e))
          (begin (evaluate (car e) env)
                 (eprogn (cdr e) env))
          (evaluate (car e) env))   ;; last return will be this
      '()))

;; this just map evaluate to all elements in e
;; use let here to enforce left to right evaluation.
;; this is not necessary always a good thing, as it impose
;; more restrictoin for a compiler to do some optimizatoin.
(define (evlis e env)
  (if (pair? e)
      (let ((argument1 (evaluate (car e) env)))
       (cons argument1 (evlis (cdr e) env)))
      '()))

;;;; environment will be implemented in association list
(define (loopup id env)
  (if (pair? env)
      (if (eq? (caar env) id)
          (cdar env)
          (lookup id (cdr env)))
      (error "No such binding")))

;; see how we manually mapping stuffs...
(define (update! id env value)
  (if (pair? env)
      (if (eq? (caar env) id)
          (begin (set-cdr! (car env) value) value)
          (update! id (cdr env) value))
      (error "No such binding")))

;; represent function with function. It feels like HOAS.
(define (invoke fn args)
  (if (procedure? fn)
      (fn args)                   ;; think, how to warn the user this error
      (error "Not a function")))  ;; as early as possible?
