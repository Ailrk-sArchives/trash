;;;; Type Systems
;; PS: 1. trick with slimv: ,i to inspect a package
;;     2. trace a function with ,t
;;     3. in the restart mannual, move cursor to a local var
;;        and press ,i to inspect local variable.
;;     4. inspect on frame with global vars like *stanard-output*
;;        to check the value of the varin that frame.
;;     5. ,xl under a name to check where the function is called
;;        ,xe check what functions are called

(declaim (optimize (speed 0) (safety 3) (debug 3)))

;;; type:
;;;   a set of objects. Types are never explicitly represented as
;;;   objects in cl, instead they are referred to by type
;;;   specifiers.
;;;   So we can talk about types, but really there are not explicit
;;;   entity that is a type.
;;;
;;;   Although being dynamically typed language, common lisp has a
;;;   complete and expressive type system. It's a strongly typed language
;;;   so no random coersion happen under the scene.
;;;
;;;   by using facilities like deftype, we can make types with various
;;;   constraints.
;;;
;;;   One thing to note is that in a dynamcially typed language,
;;;   everything are essentially an object, and everything technically
;;;   can be passed around safely unless at some call sites a type
;;;   mismatch actually cause a trouble.
;;;
;;;   In a statically typed language, we have a full set of compile time
;;;   static sytem to denote and check type's validity. If you really
;;;   boil down to the meaning of types, a type checker is a light weight
;;;   specification langauge that describes the behavior of the program.
;;;   a language get interpreted. With this simple langauge we can
;;;   interpret it at compile time, or even editing time, to check if
;;;   there is any inconsistency of the logic. The fact that all objects
;;;   come with a type with it ensure a program that pass a type checker
;;;   doesn't make type errors.
;;;
;;;   In a dynamically typed language, the story is a little bit different.
;;;   Although in common lisp we still can do statical type checking, but
;;;   common lisp as a langauge itself doesn't provide a static type
;;;   system for a type checker to work with. What usually happen is that
;;;   the compiler will collect hints from the context, and try to give
;;;   variables a type. If by deduction a variable is 100% of type a, then
;;;   it will be treated as a value with type a. Any usage that treat it
;;;   as other types will be rejected.
;;;
;;;   But this is just some compiler tricks. At it's core, type errors
;;;   are considered as runtime error. Thus the idiomatic way to ensure
;;;   the correct usage of a function usually depends on some runtime
;;;   type check facilities. In common lisp it's common to call
;;;   (check-type) to make a type assertion.
;;;
;;;   It's useful to think about how to describe an algebraic data type
;;;   in a dynamically typed langue. An algebraic data type that consists
;;;   sum types and product types. product type is just a tuple of values,
;;;   which can be represented by any sequnce like objects and fix it's
;;;   size. But to represent a value is either of form A or form B, in
;;;   a dynamically typed langauge this is actually default. Since all
;;;   value can be passed as argument of any fucntions, all stand alone
;;;   types combined make a giant sum type that coporate all possible
;;;   input. There is no need for algebraic data type.
;;;
;;;   But it's good to have a way to specify some value is either type a
;;;   or type b just for runtime checking. This can be done with some
;;;   simply custom type specifier.
;;;
;;;   Given a type A, it's possible that type B is a proper subset of
;;;   A, thus B is a subtype of A. This subtype relation can be achieved
;;;   in common lisp with struct single inheritance, or using CLOS
;;;   system.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; checking a type
(defmacro my-typep (a type)
  (subtypep (type-of a) type))

(my-typep 1 bit)
(my-typep 1 integer)
(my-typep "good" (simple-array character (4))) ;; => t
(my-typep "good" (simple-array character (5))) ;; => nil

(typep 12 '(satisfies evenp))

;; for some reason the example online uses satisfies with lambda
;; expression, but according to the document satisifies only take
;; symbols as it's argument.
;; (typep 12 '(satisfies (lambda (x) (oddp x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define types
;;; type can be defined with deftype, defstruct, defclass and defcondition

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; deftype: define a derived type specifier

;;; We can define a new type that it's value satisfies some
;;; specific predicates we defined somewhere else.
;;; From here you can see how runtime type check predicates comes
;;; hand in hand with the type itself.

;;; satisify can be used to indicate type predicate.
;;; new type defined by deftype can be used with typep
;;; satisfy : a -> bool

;;; example1: square matrix
(defun equidimensional (a)
  "equal dimensional array"
  (or (< array-rank 2)
      (apply #'= (array-dimensions a))))
(deftype square-matrix (type size)
  `(and (array ,type (,size ,size))
        (satisfies equidimensional)))

;;; type p always works
(typep (make-array '(2 2) :initial-element 0) `(square-matrix integer 2))

(defun det-2x2 (matrix)
  ;; check the type of the input paramter before we go through
  (check-type matrix (square-matrix integer 2))
  (let ((a (aref matrix 0 0))
        (b (aref matrix 0 1))
        (c (aref matrix 1 0))
        (d (aref matrix 1 1)))
    (- (* a d) (* b c))))

(let ((m (make-array '(2 2) :initial-element 0)))
  (setf (aref m 0 0) 1
        (aref m 1 1) 1)
  (format t "~A~%" m)
  (det-2x2 m))

;;; example2 prime type
(defun primep (x)
  "primality test"
  (let ((r (floor (sqrt x)))
        (isprime t))
    (if (< x 2)
      nil
      (loop for i from 2 to r do
            (when (= 0 (mod x i))
              (setf isprime nil))))
    isprime))
(deftype prime () '(and integer (satisfies primep)))
(typep 11 'prime)
(typep 12 'prime)
(typep 13 'prime)

;;; example 3 pair type
(deftype pair (a b &optional typ)
  `(satisfies (lambda (x)
                (and (consp x)
                     (typep (car x) (quote ,a))
                     (typep (cdr x) (quote ,b))))))
(typep '(1 . 2) '(pair integer integer))

;;; example 4 list type
(defun list-of-p (typ xs)
  (and (listp xs) (every (lambda (x) (typep x typ))  xs)))
(list-of-p 'integer '(1 2 3))


(deftype list-of (typ)
  `(satisfies (lambda (xs) (list-of-p (quote ,typ) xs))))

(typep '(1 2) '(list-of integer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; defstruct
;;; type defined by defstruct can also been used as a type
;;; specifier and get involved in runtime type checking.

(defstruct person
  (name "James" :type string)
  (age 0 :type integer))

(defstruct (astronaut :include person)   ;; inheritance
  (:conc-name astro-)
  (helmet-size 0.0 :type float))

(defstruct dawg
  (name "Dawg" :type string)
  (leg-num 0 :type integer))

(deftype person-or-dawg ()
  `(or person dawg))
(typep (make-dawg) 'person-or-dawg) ;; => true

(let ((p1 (make-person :name "Peter J Ladin" :age 100))
      (dawg1 (make-dawg :name "potsu" :leg-num 2)))
  (and (typep p1 'person)
       (person-p p1)
       (typecase p1     ;; this essentially allows you to pattern match.
         (person "Person!")
         (dawg "Dawg!"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; &key parameters can have a type!

(declaim (ftype (function (string &key (:n integer))) foo))
(defun foo (bar &key n) n)
;; compile time check :n to be integer
(let ((a (bar "asd" :n 1))) a)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; how to fit in type theory?
;;; https://alhassy.github.io/TypedLisp

;; t top type
(typep 'x 't)
(typep 1 t)

;; bottom type
(typep nil 'nil)

;; unit
(typep nil 'null)
(typep () 'null)

;; singleton type
(typep 3 '(eql 3))
(typep 10 '(eql 10))

;; union type
(typep 3 '(or integer char))

;; intersection type
(typep #(1) '(and array vector))

;; predicate
(typep 2 '(satisfies evenp))

;; note, many lisp types that looks polymorphic are not
;; polymorphic.
;; e.g cons is just a type that contains two parts
;; called car and cdr.

;; unlike variables in statically typed languages where
;; their types are fixed at compile time,
;; variables don't have type in lisp, only values have.

;; Because in lisp all variables have essentially the same type,
;; everything is polymorphic, which means nothing is polymorphic...

;; If everything is the same from each other, what's the point of
;; having parametric polymorphism in the first place?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; checking types

;;; in lisp, types are inferred. By inferring it means you check the
;;; type info when it matters.
;;; e.g when you are passing a string as a function parameter, which
;;; eventually will be used as an integer, it will fail somewhere in the
;;; strack trace.

;;;
;;; For a compiler, type needs to be inferred by it's context. As
;;; mentioned above, (check-type) can give compilers a good hint about
;;; what type it supposes to have.

;; tbh this type of thing is hard to do in Haskell. You need typeable
;; coerce to Maybe Integer or something, lot of hassles to come by.
(let ((ellew 321))
  (list
    (type-of ellew)
    (progn
      (setf ellew "mm")
      (type-of ellew))))

(defmacro my-checktype (v type)
  (unless (typep v type) (error "type mismatch")))

(my-checktype 1 integer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; quote to prevent evaluation
;;; x is the same as (eval x) unless specified.

(eval 1)
(eval (quote 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; coersion
;;     e : a
;; -----------------
;;  (coerce e b) : b
;; (coerce '(76 105 115 112) 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; type annotation
(+ (the integer 1)
   (the integer 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some type specifiers
;;; (vector double-float 100)
;;; (vector double-float *)     ;; leave the name unspecified
;;; (vector * 100)              ;; leave the name unspecified

;;; predicating type specifiers
;;; (satisfies numberp) is the same type as number.
;;; it's like a way to promote term level predicate function to
;;; the type level.
(typep 10 '(satisfies numberp))
(typep 10 '(satisfies integerp))
(typep 10 '(satisfies characterp))
(typep "asd" '(satisfies stringp))
(typep "asd" 'string)
(deftype string-char () '(or character (satisfies numberp)))
(deftype string-char* () '(or character (satisfies numberp)))
(typep #\a 'string-char)
(typep 2 'string-char)


;;; subtyping

(typep #(#\a #\b) '(simple-vector 2))
(typep #(#\a #\b) '(array character))
(typep #(#\a #\b) '(array character (2)))
(typep #(#\a #\b) '(array character 2))
(subtypep '(simple-vector 2) '(array character 2))
(subtypep '(simple-vector 2) '(array t))


;; you can use deftype as type alias.
(deftype array3d () '(array integer (* * *)))

(deftype mod-type (n) `(integer 0 (,n)))
(deftype list-type () '(or null cons))
(deftype square-matrix (&optional type size)
  "square-matrix includes all square 2-d arrays"
  `(array ,type (,size ,size)))

;; defstruct will introduce a new type automatically.

(declaim (ftype (function ((or integer string hash-table))) fn1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; typecase
(defun fn1 (x)
  (typecase x
    (integer (format t "~%I got an integer"))
    (string (format t "~%I got an string"))
    (hash-table (format t "~%I got an hash-table"))
    (t (format t "~%What is this"))))

(defun fn2 ()
  (fn1 (case (random 4)
         (0 5)
         (1 "a string")
         (2 (make-hash-table))
         (3 #('none 'of 'above)))))

(defun fn3 (x)
  (typecase x
    (integer (format t "~%I got an integer"))
    (string (format t "~%I got an string"))
    (hash-table (format t "~%I got an hash-table"))
    (t (format t "~%What is this"))))
