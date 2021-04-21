;; you want to sell some metals. The buyer has a
;; maximum weight limit.
;; sell the most expensive metal as much as you can to
;; maximize the profit.

;; this is just another fractional knapsack problem.
;; given i differnet metals each weigth k with prices c
;; we want to Max(Sum(k_i * c_i))


(defparameter *weight-limit* 100)
(defparameter *metals_k_c*
  '((20 . 30)
    (40 . 10)
    (14 . 45)
    (100 . 1)
    (23 . 15)))

;; sort by price, then grab the most valuabel along the way
;; until no longer fit.
(defun sell-metal (info weight-limit)
  (labels ((by-price (a b) (> (cdr a) (cdr b))))
    (let ((sorted (sort (copy-seq info) #'by-price))
          (weight 0)
          (result nil))
      (loop :while sorted :do
        (let ((m (pop sorted)))
          (if (> (+ weight (car m)) weight-limit)
              (progn
                (let ((diff (- weight-limit weight)))
                  (setf weigth weight-limit)
                  (push (cons diff (cdr m)) result)
                  (setf sorted nil)))
              (progn
                (setf weight (+ weight (car m)))
                (push m result)))))
      result)))

(sell-metal *metals_k_c* 102)
(sell-metal *metals_k_c* 35.5)
