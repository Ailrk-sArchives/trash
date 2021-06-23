;;;; Type specifiers

;; types in common lisp are represented as symbols and lists.
;; symbols are used for naming predefined classes of objects and
;; lists are used to indicate the combination of simpler types.


;; For instance, `list`, `nil`, `bit` are all type specifiers supported
;; by default.
(eql (type-of 1) 'bit)
(eql (type-of "good") '(simple-array character (4)))

;;;; type specifier lists
; (vector double-float 100)
; (vector double-float *)     ;; leave the name unspecified
; (vector * 100)              ;; leave the name unspecified

;;;; predicating type specifiers
;; (satisfies numberp) is the same type as number.
;; it's like a way to promote term level predicate function to
;; the type level.
(typep 10 '(satisfies numberp))
(typep 10 '(satisfies integerp))
(typep 10 '(satisfies characterp))
(typep "asd" '(satisfies stringp))
(typep "asd" 'string)
(deftype string-char () '(or character (satisfies numberp)))
(deftype string-char* () '(or character (satisfies numberp)))
(typep #\a 'string-char)
(typep 2 'string-char)

;;;; combines type specifiers with adat
;; yes you have adt in lisp.
(defun primep (x)
  (let ((r (floor (sqrt x)))
        (isprime t))
       (if (< x 2)
           nil
           (loop for i from 2 to r do
                 (when (= 0 (mod x i))
                       (setf isprime nil))))
       isprime))

;; just by writing a prime predicate you simply get a prime type.
(deftype prime () '(and integer (satisfies primep)))
(typep 11 'prime)   ; it actually works!
(typep 12 'prime)   ; you get rtti for free.
(typep 13 'prime)

;; subtyping

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
