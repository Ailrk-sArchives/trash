;;;; show some list data structures

(defmacro deftest (name params &rest body) `(defun ,name ,params ,@body))

;;; List

;; circular lists
(defun circular! (items)
  ;; tell the printer to recognize circular list but not print them
  ;; this gives you infinite list
  (setf *print-circle* t)
  (check-type items list)
  (setf (cdr (last items)) items))

(deftest
  circular-test ()
  (let ((xs '(1 2 3)))
    (circular! xs)
    (format t "~%circular-test~%")
    (format t ":xs ~a~%" xs)
    (format t ":5th xs ~a~%" (fifth xs))))


;; dotted list ()
(defun dotted-list ()
  (let ((t1 (cons 1 2))
        (t2 (cons  1(cons 2 nil))))
    nil))

;; basic car cdr frsit/rest, nth
(deftest
  basic-list ()
  (format t "~%basic-list~%")
  (let ((xs '(1 2 3 4 5)))
    (format t ": ~a~%" (car xs))    ;; => 1
    (format t ": ~a~%" (cdr xs))    ;; => (2 3 4 5)

    (format t ": ~a~%" (fifth xs))  ;; => 5
    (format t ": ~a~%" (second xs)) ;; => 2

    (format t ": ~a~%" (nthcdr 3 xs)) ;; => (4 5)

    (format t ": ~a~%" (last xs))       ;; => (5) return the last con cell
    (format t ": ~a~%" (butlast xs))    ;; => (1 2 3 4)
    (format t ": ~a~%" (nbutlast xs 2)) ;; => (1 2 3)


    (format t ": ~a~%" (nbutlast xs 2)) ;; => (1 2 3)

    (format t ": ~a~%" (reverse xs)) ;;=> (5 4 3 2 1)

    (format t ": ~a~%" (append xs xs)) ;;=> (1 2 3 4 5 1 2 3 4 5)

    nil))

(deftest
  destructive-list ()
  ;; nreverse will flip the direction of
  ;; cdr of the con cell and return the last element
  ;; the original concell will become the head of a
  ;; list with only one element.
  (let* ((xs '(1 2 3 4 5))
         (sx (nreverse xs)))
    (format t "~%nreverse~%")
    (format t "sx: ~a~%" sx)   ; => (5 4 3 2 1)
    (format t "xs: ~a~%" xs)   ; => (1)
    nil)

  (let* ((xs '(1 2)))
    (format t "~%push~%")
    (push 1 xs)
    (push 1 (elt xs 1))
    (push 1 (elt xs 2))
    (format t "xs: ~a~%" xs)
    (pop xs)
    (format t "xs: ~a~%" xs))

  (destructuring-bind (x y z) (list 1 2 3)
    (format t "~%dest:ructuring-bind~%")
    (format t "x: ~a~%" x)))

(deftest
  list-predicates ()
  (let ((xs '(1 2 3)))
    (format t "~%predicates~%")
    (format t ": ~a~%" (null xs))
    (format t ": ~a~%" (listp xs))
    (format t ": ~a~%" (atom xs))))

(deftest
  list-membership ()
  (let ((xs '(1 2 3))
        (ys '("a" "b" "c")))
    (format t ": ~a~%" (member 2 xs))
    (format t ": ~a~%" (member "b" ys :test #'equal))
    (format t ": ~a~%" (elt xs 2))))

(deftest
  list-creation ()
  (format t ": ~a~%" (make-list 10 :initial-element "p")))

(deftest
  list-sub ()
  (format t ": ~a~%" (subst 'one 1 '(1 2 3)) :test #'equal)

  (format t ": ~a~%" (sublis '((x . 10) (y. 20)) '(xyx)))


  (format t ": ~a~%" (sublis '((x . 10) (y . 20)) '(λ x \. λ y \. x + y)
                             :test #'equal))

  (format t ": ~a~%" (sublis '((x . 10) (y . 20)) '(* x (+ x y) (* y y))))

  (format t ": ~a~%" (sublis '((t . "foo")) '("one" 2 ("three" (5 2)))
                             :key #'stringp))

  )


;;; sequences

;;; functions

;;; set

;;; fset

;;; sizes

;;; vectors

;;; hashtable

;;; alist

;;; plist

;;; structure

;;; tree

