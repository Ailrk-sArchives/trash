;; select nth biggest element in a sequence.


;; calcualte the pivot
(defun get-pivot-idx (xs) (floor (/ (length xs) 2)))

;; move elements smaller then p to the left, others to the right.
;; the running time is O(n).
(defun partition (xs p)
  (declare (type sequence xs)
           (type integer p))
  (let ((left nil)
        (right nil))
    (loop for i from 0 to (- (length xs) 1) do
          (cond ((< (elt xs i) (elt xs p))
                 (setf left (cons (elt xs i) left)))
                ((> (elt xs i) (elt xs p))
                 (setf right (cons (elt xs i) right)))))
    (values left right)))

(defun test-partition ()
  (partition '(7 3 4 2 1 0 5) 3))

;; select a pivot, if there are eactly k - 1 elements on the left hand side,
;; that means k is the kst biggest element.
;; otherwise, if length of left > k - 1, k is in the left hand side.
;; recurse on left.
;; if lenght of left < k - 1, k is in right hand size, recurse on right hand.
(defun kselect (xs k)
  (declare (type sequence xs)
           (type integer k))
  (if (< (length xs) 5)
      (elt (sort xs #'>) (- k 1))
      (let ((p (get-pivot-idx xs)))
        (multiple-value-bind (left right) (partition xs p)
          (progn
            (cond ((eql (- k 1) (length left)) (elt xs (- k 1)))
                  ((> (length left) (- k 1)) (kselect left k))
                  ((< (length left) (- k 1))
                   (kselect right (- k (length left) 1)))))))))

(defun test-kselet ()
  (let ((ls '(7 3 5 4 1 0 2)))
    (format t "~a~%" (kselect ls 1))
    (format t "~a~%" (kselect ls 2))
    (format t "~a~%" (kselect ls 3))
    (format t "~a~%" (kselect ls 4))
    (format t "~a~%" (kselect ls 5))
    (format t "~a~%" (kselect ls 6))
    (format t "~a~%" (kselect ls 7))))
