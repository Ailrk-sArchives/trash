;; break up multiplication
;; formulate mulitplication as a divide and conquer
;; problem


;; Spliting an integer into two parts.
;; [x1 x2 ... xn] = [x1 x2 x3 ... xn/2] * 10^n/2 = [xn/2+1 xn/2+2 ... xn]
;; xy = (a*10^n/2 + b)(c * 10 n/2 + d)
;;    = (ac)10^n + (ad + bc)10n/2 + db
;; note, a, b, c, d all have n/2 digits, so n/2 input
;; we need 4 smaller multiplication to calculate the problem.
;; This forms a recursion tree.


(defstruct splited
  (a 0 :type integer)
  (b 0 :type integer)
  (n/2 0 :type integer))


(defun chop (x)
  (let* ((xstr (write-to-string x))
         (n/2 (floor (length xstr) 2))
         (a (parse-integer (subseq xstr 0 n/2)))
         (b (parse-integer (subseq xstr n/2 (length xstr)))))
    (make-splited :a a :b b :n/2 n/2)))


(defun mult-chopped (x y)
  (let* ((x1 (chop x))
         (y1 (chop y))
         (a (splited-a x1))
         (b (splited-b x1))
         (c (splited-a y1))
         (d (splited-b y1))
         (n/2 (splited-n/2 x1)))
    (if (not (= (splited-n/2 x1)
                (splited-n/2 y1)))
        (error "only support multiplying two number with the same # of digits"))
    (+ (* a c (expt 10 (* 2 n/2)))
       (* (+ (* a d) (* c b)) (expt 10 n/2))
       (* b d))))
