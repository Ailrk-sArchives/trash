;; shortest job first is actually a classical example for greedy algorithm.

;; we want to minimize the average of the completion time.

;; NOTE we assume all jobs are known ahead of the time.
(defparameter *jobs*
  '((a . 23)
    (b . 56)
    (c . 4)
    (d . 12)
    (e . 98)
    (f . 1)
    (g . 2)))


;; we know each time if we choose the shortest job, we can minimize the overall
;; completion time.


(defun shortest-job-first (jobs)
  (labels ((by-length (a b)
             (< (cdr a) (cdr b))))
    (sort (copy-seq jobs) #'by-length)))

(defun accumulate (xs)
  (let ((order (copy-seq xs)))
    (loop :for i :from 1 :to (- (length order) 1) :do
          (setf (cdr (elt order i)) (+ (cdr (elt order i))
                                       (cdr (elt order (- i 1))))))
    order))

;; it mutates somehow...
(accumulate (shortest-job-first *jobs*))


;; How to prove this is the minimal average completion time you can get?
;; simply prove by contradiction.
;; Assume you complete all jobs with an order that task p1 with longer
;; runtime start earily than task p2.
;; then you just need to find a way to prove there is a better way to
;; decrase the aeverage time.
;; this is always possible, just formulate the runtime and prove
;; swap them gives a better average.
