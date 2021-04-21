;; edit distance problem

;; string x & y, what's the cheapest possible (character edits)
;; to turn x -> y
;; first define character edits:
;; 1. insert c
;; 2. delete c
;; 3. replace c

;; 1. subproblem = edit distance on x[i:], y[j:] for all i, j
;;    # subproblems = O(|x| |y|)
;; 2. guess (use the edit operations):
;;        x: ....
;;           i
;;        y: ....
;;           j
;;    (what to do to make the first character of x become the
;;     first charaacter of y?)
;;    choice 1: replace x[i] with y[j]
;;    choice 2: insert x with y[j]
;;    choice 3: delete x[i]
;; 3. recurrence
;;      DP(i, j) =  min {
;;          (cost of replace x[i] -> y[j]) + DP(i+1, j+1),
;;          (cost of insert x[i] with y[j]) + DP(i, j+1),
;;          (cost of delete x[i] with y[j]) + DP(i+1, j),
;;      }
;; 4. topological order (shortest path in the DAG)
;; 5. DP(0, 0)


(defparameter *x* "abcjladsfsdlj")
(defparameter *y* "uhabdkvbkd")

(defun e-delete (xs))
(defun e-insert (xs))
(defun e-replace (xs))

(defun edit-distance (xs)

  )
