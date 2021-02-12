;;;; I have no idea how a loop dsl can be so complicated...
;; but it's a way to investigate what doesn it mean to be a loop so...
;; guess it's worthy.

;; this is really a zip
(loop
  for item in '(a b c d e)
  for i from 1 to 10
  do (format t "(~a, ~a)" item i))

;; this is the cartesian product.
(loop for item in '(a b c d e) do
      (loop for i from 1 to 10 do
            (format t "(~a, ~a)" item i)))

;;;; counting loop (the most primitive loops
;; Three main preprositional phrases:
;;   fromwhere:  from, downfrom, upfrom
;;   towhere:    to, upto, below, downto, above
;;   byhowmuch:  by ...
;; you must specify at least on of these three prepositions.

;; default starts at 0.
(loop for i upto 10 collect i)
(loop for i to 10 collect i)

;; downto requires a starting value.
(loop for i from 10 downto 1 collect i)
(loop for i from 10 downto -10 collect i)

;; can start from any value
(loop for i from 20 to 30 collect i)
(loop for i from 20 to 30 by 2 collect i)

;; need a form after by, must return a positive number
(loop for i from 20 to 30 by (floor 24 11) collect i)

;; loop is a macro, so it must know the direciton of the loop
;; to know how to step it's variable.
(loop for i from 20 to 10 collect i)  ;; this won't run since inc is default.
(loop for i from 20 downto 10 collect i)

;; simply repeat
(loop repeat 3 do (format t "good"))

;;;; looping over collection and packages (for each)
;; Only two prepositional phrases:
;;  :in
;;  :on

;; :in iter over a list
(loop for i in (loop for i from 0 to 10 collect i) by #'cddr collect i)
(loop for i in (loop for i from 0 to 30 collect i) by #'cdddr collect i)
(loop for i in (loop for i from 0 to 100 collect i)
      by (lambda (x) (cdddr (cdddr x)))
      collect i)

;; :on step over con cell. it's like scandl (:) []
(loop for x on '(10 20 30 40 50) collect x)
(loop for x on '(10 20 30 40 50) by #'cddr collect x)

;; looping over vector uses different keywords somehow
(loop for x across "abcd" collect x)
(loop  for x across #(1 2 3 4 5) collect x)

;; looping over hashtable
(let ((table (make-hash-table)))
  (setf (gethash 'a table) 1)
  (setf (gethash 'b table) 2)
  (setf (gethash 'c table) 3)
  (loop for k being the hash-keys in table using (hash-value v) do
        (format t "~a ~a / " k v)))

;;;; Equal then iteration
;; : init-form
;; : then-step

;; be careful about the order.

;; here y uses the value of x from the last iteration.
;; x y
;; 0 1
;; 1 2
;; 2 4
(loop repeat 5
      for x = 0 then y
      for y = 1 then (+ x y)
      collect y)

(loop repeat 5
      for y = 1 then (+ x y)
      for x = 0 then y
      collect y)

;; here y uses the value of x at current iteration
(loop repeat 5
      for x = 0 then y
      and y = 1 then (+ x y)
      collect y)

;;;; local variables

(loop for x upto 3
      with foo = :foo
      and bar = :bar
      collect (list x foo bar))

(loop for x upto 3
      with foo = :foo
      with bar = :bar
      collect (list x foo bar))

;;; destructring variables

;; accetps destructuring binding
(loop for (a b) in '((1 2) (3 4) (5 6)) collect (+ a b))

;; accepts dotted list.
(loop for (item . rest) on '(1 2 3 4 5 6) do
      (format t "~a ~A ~%" item rest))

(loop for cons on '(1 2 3 4 5 6) do
      (format t "~a" (car cons)) when (cdr cons) do
      (format t ", "))
;; above is the same as this
(loop for (item . rest) on '(1 2 3 4 5 6)
      do (format t "~a" item)
      when rest do (format t ", "))     ;; don't add , after nil.

;;;; value accumulation clause

(let ((randomlist (loop repeat 100 collect (random 10000))))
  (loop for i in randomlist
        counting (evenp i) into evens
        counting (oddp i) into odds
        summing i into total
        maximizing i into max
        minimizing i into min
        finally (return (list min max total evens odds))))

(defun primep (x)
  (let ((r (floor (sqrt x)))
        (isprime t))
       (if (< x 2)
           nil
           (loop for i from 2 to r do
                 (when (= 0 (mod x i))
                       (setf isprime nil))))
       isprime))

(defun primep (x)
  (let ((r (floor (sqrt x)))
        (isprime t))
       (if (< x 2)
           nil
           (loop for i from 2 to r do
                 (when (= 0 (mod x i))
                       (setf isprime nil))))
       isprime))

(let ((randomlist (loop repeat 100 collect (random 10000))))
  (loop for i in randomlist
        count (evenp i) into evens
        count (oddp i) into odds
        count (primep i) into primes
        sum i into total
        maximize i into max
        minimize i into min
        finally (return (list :evens evens
                              :odds odds
                              :primes primes
                              :total total
                              :max max
                              :min min))))

; side: return ==  (return-from nil)
(block nil (return) 1)
(block nil (return 1) 2)
(block alpha (block nil (return 1)) 2) ; return from nil and execute alpha

;;;; unconditional execution
;; return clauses (different from return and return-from
; break out of a loop with return
(block outer
       (loop for i from 0 return 100)
       (print "print this")
       200) ; => 100

(block outer
       (loop for i from 0 do (return-from outer 100))   ;; return from outer block
       (print "print this")
       200) ; => 200

(block outer
       (loop for i from 0 to 100 do
             (when (>= i 20)
                 (return-from outer i)))
       (print "print this")
       200) ; => 20

;;;; conditional executin

;; this can be written as
(loop for i from 1 to 10 do (when (evenp i) (print i)))
;; this
(loop for i from 1 to 10 when (evenp i) sum i)

;; the return when i > 20 loop can also be written like this
(block outer
       (return-from outer (loop for i from 0 to 200 when (>= i 20) return i))
       (print "print this"))

;;;; use it to refer to selected value
(let ((table (make-hash-table)))
  (setf (gethash 'a table) 1)
  (setf (gethash 'b table) 2)
  (setf (gethash 'e table) 3)
  (loop for key in '(a b c d e) when (gethash key table) collect it))

;;;; loop blac belt. It's really a dsl if you look at it now...
;; it has value binding
;; it has loop (the whole thing is a loop
;; it has conditional (complicated control flow, even goto)
;; it can combine multiple expression together
;; it's just another language live in common lisp standard, as template to c++.
(loop for i from 1 to 100
      if (evenp i)
      minimize i into min-even and
      maximize i into max-even and
      unless (zerop (mod i 4))
      sum i into even-not-fours-total
      end
      and sum i into even-total
      else
      minimize i into min-odd and
      maximize i into max-odd and
      when (zerop (mod i 5))
      sum i into fives-total
      end
      and sum i into odd-total
      do (return (list min-even
                        max-even
                        min-odd
                        max-odd
                        even-total
                        odd-total
                        fives-total
                        even-not-fours-total)))

;; loop setting up and tear down.
;; use initially and finally set up and tear down states for the loop.


;; termination test
;; :termination clauses while, until always never thereis
;; these clauses  decide when to exit a loop

(if (loop for n in '(2 2 2 4 5 6) always (evenp n) do ;; will only loop 4 times
          (prin1 "looping"))
    (print "all even"))

(if (loop for n in (loop for i from 0 to 10 by 2 collect i) never (oddp n))
    (print "all even")u)

;; check if there exists
(loop for char across "abcdef" thereis (digit-char-p char))
(loop for char across "abc1ef" thereis (digit-char-p char))


;;;; Conclusion
;; loop clauses structure
;; 1. name clause with
;; 2. general control clauses (initally, with, for repeat)
;; 3. body clauses (conditional/unconditional clauses, accumulation, termination test)
;; 4. end with finally
