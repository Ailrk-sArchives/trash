;; Analysis on the compleixty of Multiplication

;; TODO karatsuba

;; note common lisp is 2 lisp, value and functions are in different namespaces.

;;;; hand written multiplication
;; assume a and b has same digit
;; grade school multiplication is really a string algorithm...
;;      54
;;    x 28
;;   -------
;;  each digit in one number needs to multply with each digit in the
;;  other one,
;;  O(n^2)


; (defun one-digit-mult (a b)
;   (declare (type sequence a)
;            (type integer b))
;   (cond
;     ((< (length a) 1) (error "a must be at least one digit"))
;     (t (let* ((carry 0)
;               (buffer '()))
;          (loop for n in (reverse a) do
;                (let ((nb (* n b)))
;                  (multiple-value-bind (q r) (floor (* nb) 10)
;                    (progn
;                      (setf buffer
;                            (cons
;                              (if (= carry 0) r
;                                  (progn
;                                    (multiple-value-bind
;                                      (q1 r1) (floor (+ nb carry) 10)
;                                      (setf carry q1)
;                                      r1)))
;                              buffer))))))))))

; (one-digit-mult '(1 2 3) 9)



(defun mult-one (as a)
  (declare (type list as)
           (type integer a))
  (let ((result nil)
        (carry 0)
        (sa (reverse as)))
    (loop for n in sa do
          (let* ((product (+ carry (* n a)))
                 (r (mod product 10))
                 (q (floor product 10)))
            (if (> q 0) (setf carry q) (setf carry 0))
            (setf result (cons r result))))
    (format t "~A" result)))

(mult-one '(1 2 3) 9)

(loop for i from 0 to 10
      for j from 0 to 10
      for k = 0 do
      (format t "~a ~a ~a" i j k))
