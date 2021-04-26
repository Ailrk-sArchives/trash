(defpackage #:matching-bipartite
  (:use :common-lisp))

(in-package #:matching-bipartite)

;; https://en.wikipedia.org/wiki/Matching_(graph_theory)#:~:text=A%20perfect%20matching%20is%20a,term%20complete%20matching%20is%20used.
;; Matching is a graph theory term for saying an set of edges without common
;; vertices.

;; Finding a matching in a bipartite graph can be seen as a network flow problem.

;; 1. maximal matching: matching M that is not a subset of any other matching.
;; 2. maximum matching: matching M that contains largest possible number of edges.
;; 3. perfect matching: matches all vertices in a graph.
