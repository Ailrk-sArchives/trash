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


(defun mult-one (as a)
  (declare (type list as)
           (type integer a))
  (let ((result nil)
        (carry 0)
        (sa (reverse as)))
    (loop for n in sa
          for i from 0 to (length sa) do
          (let* ((product (+ carry (* n a)))
                 (r (mod product 10))
                 (q (floor product 10)))
            (if (> q 0) (setf carry q) (setf carry 0))
            (setf result (cons r result))
            (if (and (= i (- (length sa) 1)) (not (= carry 0)))
                (setf result (cons carry result)))))
    result))

;; (format t "~A~%" (mult-one '(1 2 3) 8))
;; (format t "~A~%" (mult-one '(1 2 3) 7))

(defun collect-intermediates (a b)
  (declare (type integer a)
           (type integer b))
  (let* ((as (loop for n across (write-to-string a) collect (digit-char-p n)))
         (bs (loop for n across (write-to-string b) collect (digit-char-p n)))
         (intermediates (loop for b* in (reverse bs) collect (mult-one as b*))))
    intermediates))

;; (format t "~A~%" (collect-intermediates 123 98))

(defun pad-zero (xxs)
  (declare (type sequence xxs))
  (let ((maxlen (+ (length (elt xxs (- (length xxs) 1)))
                   (- (length xxs) 1))))
    (labels ((pad-zero (xs i)
               (append (make-list (- maxlen i (length xs)) :initial-element 0)
                       xs
                       (make-list i :initial-element 0))))
      (mapcar #'(lambda (xs)
                  (parse-integer
                    (format nil "~{~A~}" xs)))
              (mapcar #'pad-zero xxs
                (loop for i from 0 to (length xss) collect i))))))


(format t "~A~%" (pad-zero (collect-intermediates 123 98)))

(defun mult-by-hand-* (a b)
  (loop for n in (pad-zero (collect-intermediates a b)) summing n))

(defun mult-by-hand (a b)
  (if (> a b) (mult-by-hand-* a b) (mult-by-hand-* b a)))

(format t "~A~%" (mult-by-hand 123 98))
