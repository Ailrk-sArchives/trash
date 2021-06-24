;;;; common collections

;;; Lisp was designed to be list processing langauge, but as
;;; time goes, many more efficient data sturcture creeps into
;;; the language.
;;; It doesn't matter what paradigm you are using, there will
;;; always be a need for a efficient data structure. Even in
;;; haskell sometimes you want a mutable array to play with.


;;; Other than list, common lisp has some other builtin types
;;; that comes very handy. Not like in haskell which even a map
;;; needs to come as a library, common lisp has the most handy
;;; contains available all the time. I really think haskell should
;;; do the same.

;;; Most commonly used:
;;; 1. array
;;; 2. vector
;;; 3. hashtable
;;; 4. struct

;;; These variation is more than enough for daily uses. Other things
;;; are just optmization on specific use cases.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; struct
;;; one problem of struct is one they are created they cannot
;;; be redefined.
;;; if you are using an repl to develop struct, you should be careful
;;; compiling struct because if you want to modify anything after it's
;;; loaded, the only senible way is to restart the repl.
(defstruct (human (:constructor create-person (id name age)))
  id name age)

;;; structs can have single inheritance, works as you expected.
(defstruct (female (:include human)) (gender "female" :type string))

(let ((me (create-person 1 "me" 10))
      (notme (make-female :id 10 :name "notme" :age 20)))
  (format t "This human is ~a~%" (human-name me))
  (format t "This female is ~a~%" (female-name notme))
  (format t "This human is also ~a~%" (human-name notme))
  (format t "am I female? ~a" (female-p me))
  (format t "female is a human: ~a" (subtypep 'female 'human)))
