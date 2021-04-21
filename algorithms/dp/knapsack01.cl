;; classic dp
;; given weigths and values of nitems, put in sack as much as
;; possible. You cannot break items.

;; 1. subproblem
;;      maximize suffixes of items [i:] & remaining capacity x <= s
;; 2. guessing
;;      is item i in subet or not 0 or 1
;; 3. reccurence
;;      DP(i) = max {
;;        DP(i + 1, x), DP(i+1, x -s) + vi
;;      }
;;
;; 4. topological order
;; 5. DP(

(defun knapsak01 ()

  )
