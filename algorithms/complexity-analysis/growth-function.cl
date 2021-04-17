;; We use order of growth of the running time of an algorithm to determine
;; it's complexity. (Asymptotic analysis)
;; This approach simplify the work of analyzing algorithm significantly.

;;;; Θ notation ;;;;
;;  Θ(g(n)) = { f(n) | ∃c₁, c₂ ∈ ℤ⁺ st
;;                     0 ≤ c₁g(n) ≤ f(n) ≤ c₂g(n) ∀ n ≥ n₀ }
;;  we say g(n) is an asymptotic tight bound of f(n)

;;;; O notation ;;;;
;;  O(g(n)) =  { f(n) | ∃c, n₀ st 0 ≤ f(n) ≤ cg(n) ∀ n ≥ n₀ }
;;  cg(n) is the upper bound of f(n)

;;;; Ω notation ;;;;
;; Ω(g(n)) = { f(n) | ∃c, n₀ st o ≤ cg(n) ≤ f(n) ∀ n ≥ n₀ }
;; cg(n) is the lower bound of f(n)

;;;; Them.1 f(n) == Θ(g(n)) ⇔ f(n) = O(g(n)) ∧ f(n) = Ω(g(n))
;; If cg(n) can bound f(n) above, then they must growth in the same rate.
;; because if f(n) growth faster, eventually it will be larger.
;; it's the same for cg(n) bound f(n) below.
;; so if g(n) can bound both above and below, apparently f(n) is bounded
;; by g(n). So we can analyse the complexity of f(n) by analysing g(n)

;;;; interpretation ;;;;
;; 2n² + 3n + 1 =
;; 2n² + θ(n) =
;; θ(n²)
;; read as
;; ∃ f(n) ∈ θ(n) st 2n² + 3n + 1  = 2n² + f(n)
;; ∃ g(n) ∈ θ(n²) st 2n² + f(n) = g(n)

;;;; little o
;; O(g(n)) can be not asymptotically tight. We can denote all upper bound that's not
;; asymptotically tight as o(g(n))
;; e.g 2n = o(n²), but 2n ≢ o(n).
;;;; little ω is the same concept.

;;;; exponential function grows faster then polynomial function.

;;;; Super slow growth function, iterated logrithm
;; lg*n = min {i ≥ 0 | lgⁱn ≤ 1}

;;;; How to obtain the order of growth of recurrences algorithm? ;;;;
;; three methods to find complexity of recurrence algo.
;; 1. substitition method
;; 2. recursion tree method
;; 3. master method

;;;; Substitution method
;; 1. guess a bound
;; 2. prove the bound with math induction.
;;    - you need to find a proper base case first.
;;    - this process is pretty heuristic based.

;;;;  Recursion tree
;; 1. draw the tree and do some sloppy analysis to yield a good guess.
;; 2. use substitution method to proof the guess.

;;;; Master method T(n) = aT(n/b) + f(n)  *
;; just apply three cases
;; T(n) = aT(n/b) + f(n)

;;                  log'b(a) - ε                     log'b(a)
;; case1: f(n) = O(n),  ε > 0           ⇔  T(n) = θ(n)

;;                  log'b(a)                         log'b(a)
;; case2: f(n) = O(n)                   ⇔  T(n) = θ(n          . lg(n))

;;                  log'b(a) + ε
;; case3: f(n) = O(n),   ε > 0          ⇔  T(n) = θ(f(n))
;;                   ,  af(n/b) ≤ cf(n)
;;                      for c < 1
;;                      and n large enough

;; Example
;; 1. T(n) = 9T(n/3) + n:
;;  a = 9, b = 3, f(n) = n
;;
;;   log'3(9)
;;  n         = θ(n²)
;;                     log'3(9) - ε
;;  we find f(n) = O(n             ) where ε = 1
;;  so we can apply case 1 of master theorem.
;;
;;            log'3(9)
;;  T(n) = θ(n        )  = θ(n²)
