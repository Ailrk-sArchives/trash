;; Latex text adjustment

;; how to solve:
;; 1. subproblems: suffixes workds [i:]
;; 2. guess where to start 2nd line #choices <= n-1 = O(n)
;; 3. recurrence:
;;     DP(i) = min{
;;       DP(j) + badness(i, j) forall j in range(i+1, n+1)
;;     }
;; 4. check topological order
;; 5. total time: O(n^2)  // (number of subproblems * running time for subproblem)
;; 6. original problem DP(0)

;; use parent pointer to remember the best guess.
;; like backtrace.

;; todo

(defparameter *works*
  (concatenate
    'string
    "sdl laj al klajsdf alks aslkdfd s iojwe awbrbk hker asidj awei"
    " k ioa iowe hwef nq lkaj foiua aiosudf aiosud oifausd hafshakd"
    "o asdj dif anwefn oi sd fajsd oipfe ls oaiusdo nkadwoei oiu jsd"))

(defparameter *page-width* 40)


;; where to cut the line to fit the document best?

(defun badness (str pw tw)
  "badness of a substring"
  )


(defun text-justification ()

  )
