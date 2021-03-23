;; just some stuffs you already seen

;;;; lisp 2, function and variables have different namespaces.

(defun foo (x) (+ x 1))
(setf foo 10)

;; select value of a symbol from different namespace.
(symbol-value 'foo)
(symbol-function 'foo)

;; this is essentially defun under the hood.
(setf (symbol-function 'bar)
      (lambda (x) (* x x)))
(bar 2)

;; apply takes argument list, funcall call function directly.
(apply #'+ '(1 2))
(apply (symbol-function '+) '(1 2))
(funcall #'+ 1 2)

(mapcar (lambda (x) (+ x 10)) '(1 2 3))

(remove-if-not #'evenp '(1 2 3 4 5))

;; functions as properties
(defun behave (animal)
  (funcall (get animal 'behavior)))

;; All symbol can have global property list.
;; here we set the plist of 'dog direcly
(progn
  (setf (get 'dog 'behavior) #'(lambda () (format t "wig tails")))
  (behave 'dog)
  (symbol-plist 'dog))

;;;; plist, function, and value are i different name spaces!

(symbol-plist 'cat)   ;; at beginning it's default empty
(get 'cat 'mew)       ;; get from symbol plist.
                      ;; can give default value, this is different from getf.
(setf (get 'cat 'mew) "mew")    ;; set the global plist.

(setf cat 1)
(symbol-value 'cat)     ;; the value namespace

(defun cat () (format t "~a~%" (get 'cat 'mew)))
(funcall #'cat)         ;; the function namespace

;;;; under the hood get looks like this.
(defun get-* (x y) (getf (symbol-plist x) y))

(setf al '((a . 1) (b . 2)))

;; a explicit property list.
(defun behave1 (animal)
  (funcall (getf animal :behavior)))
(progn
  (let ((dog '()))
    (setf (getf dog :behavior) #'(lambda () (format t "wig tails")))
    (behave1 dog)))

(defun our-length (lst)
  (labels ((rec (lst acc)
             (if (null lst)
                 acc
                 (rec (cdr lst) (1+ acc))) ))
    (rec lst 0)))

;;;; function can be compile together or individuely.
;; you can even call a function to compile them.

(defun foo (x) (1+ x))
(compile 'foo)  ;; compile the function cell of the symbol 'foo



;;;; Conclusion
;; lisp is based on s expression
;;
;; you have either an atom or a list
;;
;; an atom can be a literal value, or a symbol.
;;
;; literal value is like string, integer, lambda  etc.
;;
;; symbol is it's own datatype, like a string but not, in the sense
;; that it's the symbol used in ast directly.
;;
;; a symbol can bind to a value. once bind you can access the value from
;; the symbol
;;
;; each symbol has two cells, one for function one for variable.
;;
;; a list is either a list or a function call
;;
;; special forms like if else are cases that treated differently.
;;
;; macro are functions execute at compile time.
;;
;; if macro return a list, the list itself as the ast of the code can be
;; substituted into wherever it's called. and that's how you are able to
;; write dsl with macros.
;;

;;;; further
;; common lisp is heavily imperative, there are lots of functions are mutation
;; based.
;;
;; You have wierd condition system that separate error handling into three parts:
;; handler, signaling error, and choose handler to handle errors.
;; handler are called restart. you can define different restarts.
;;
;; There is a widely used object system called CLOS. It has different inheritence
;; mechanism as normal oo. Namely it's based on generic function and is
;; able to do multi-disptaching.
;;
;; In clos class are only for data. generic funtion is like a interface for all
;; methods that has the same signature. Each implemnetation of a method specialize
;; for one type. You have some operators to interact with other methods implement
;; the generic function. like `call-next-method` etc.
;;

;;;; usage
;; plist and alist are very fundamental light data structures used in lisp. Both of
;; them had quite some history.
;;
;; function can have multiple return values. By default the first one is returned, but
;; you can disctruct multiple values with multiple-value-bind
;;
;; struct is a very light weight data structure, you can use it without worrying anything
;;
;; vector and hashmap ae properly supported too.
;;

;;;; typing
;; common lisp is strongly typed dynamic language. But it's also compiled, you
;; are able to annotate some types to assist compiler better.
;;
;; some functions around types like typep, typecase are convinent. Thye also
;; implies runtime type information.
;;
;; if you annotate type, compile will give you better warning. If you call a
;; annotated funtion with the runtime, it will be a runtime type error.

;;;; dsl
;; lisp is famous for it's dsl ability. The ability is mainly comes from the
;; homoiconcity of data and code. To exploit the homoiconicity, you write macro
;; to generate code.
;;
;; code generate code also called mata function
;;
;; There can be multiple stages for a macro. You can nest quotes and unquotes.
