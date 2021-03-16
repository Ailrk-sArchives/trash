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

(defun grade-school-multiplication (a b)
  (let*
   ((stra (reverse (write-to-string a)))
    (strb (reverse (write-to-string b)))
    (len (length stra))
    ;; to make the vector mutable we needs to set fill-pointer.
    (intermediates (make-array len :element-type 'string :fill-pointer 0))
    (padding -1)) ;; (loop for from 0 to -1 return nil)
   (labels
    ((once (digit)  ; digit * stra
           (let
            ((carry 0)
             (acc ""))
            (loop for sa across stra do
                  (let*
                   ((a (parse-integer (string sa)))
                    (product (if (= carry 0)
                                 (* a digit)
                                 (+ (* a digit) carry))))
                   (if (> product 10)
                       (setf carry (floor product 10))
                       (setf carry 0))
                   (setf acc (concatenate 'string
                                          (write-to-string (mod product 10))
                                          acc
                                          (format nil "~{~A~}"
                                                  (loop for i from 0 to
                                                        padding collecting "0"))))))
            (vector-push acc intermediates))))
    (when (not (= (length stra) (length strb)))
          (error "lenth a and b needs to have same digits"))

    (loop for sb across strb do
          (progn
           (once (parse-integer (string sb)))
           (incf padding)))
    (loop for n across intermediates summing (parse-integer n)))))
