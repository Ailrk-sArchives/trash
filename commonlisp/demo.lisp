#|
    learn some common lisp today.
|#

; single line comment

(defun meaning (life)
  (let ((meh "abc"))
    (loop :for x :across meh  ; store values into x then return it.
          :collect x)))

(meaning '())

; creates a symbol from a string
(intern "AAAA")

(let ((me "dance with you")) me)

;; Structs
(defstruct dog name breed age)
(defparameter *rover*
  (make-dog :name "rover"
          :breed "collie"
          :age 5))

(character-to-morse #\a)

*rover*
(dog-p *rover*)
(dog-name *rover*)

;; pairs
(cons 'subject 'verb)
(car (cons 'subject 'verb))
(cdr (cons 'subject 'verb))

;; list
(cons 1 (cons 2 (cons 3 nil)))
(list 1 2 3)

(concatenate 'list '(1 2) '(3 4))

#(1 2 3)

(make-array (list 2 2))

; create a hashtable
(defparameter *m* (make-hash-table))

(setf (gethash 'a *m*) 1)

(gethash 'a *m*)

; functions
(lambda () "hello world")

(defun helloworld () "hello world")
(defun hello (name) (format nil "Hello, ~A" name))
(hello "good")

; optional arguments
(defun hello- (name &optional from)
  (if from
      (format t "Hello, ~A from ~A" name from)
      (format t "Hello, ~A" name)))
(hello- "good")
(hello- "good" "me")


(defun fact1 (n)
  (if (< n 2)
      1
      (* fact1 (- n 1))))

(fact1 5)

(defun (lamdba (x)
               (fact0)))
