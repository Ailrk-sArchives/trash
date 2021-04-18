;;;; Strongly connected components.

;; it's not necessarily for you to be able to reach all nodes in a
;; graph from a single starting point.
;; if a directed acyclic graph has multiple strongly connected components,
;; you need to do multiple dfs from  0-in degree nodes.

;; scc is a equivalence class over "mutually reachable" (say Reach) equivalence
;; relation
;; this just means ssc has
;; 1. reflexivity     Reach(a, a)
;; 2. transitivity    Reach(a, b) Reach(b, c) <=> Reach(a, c)
;; 3. symmetric       Reach(a, b) <=> Reach(b, a)


(defun scc ()
  )

