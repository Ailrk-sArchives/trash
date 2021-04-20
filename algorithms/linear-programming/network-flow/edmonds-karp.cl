;; a specialization of furd-fulkerson.
;; find augmenting path with bfs

;; ford fulkerson algorithm to find the maximum flow.
;; f(u, v) <- for all (u, v)
;; while an augmenting path P in residual network Gf exists,
;; do augment f by cf(P)

;; invaraints to maintain:
;; 1. capacity constraints
;; 2. skew symmetry
;; 3. flow conservation

(defpackage #:edmondskarps
  (:use "COMMON-LISP"))

(in-package #:edmondskarps)


(defmacro init-hash-table (xs)
  `(let ((m (make-hash-table)))
     (dolist (kv ,xs)
       (setf (gethash (car kv) m) (cdr kv)))
     m))


(defparameter *flownetwork*
  (init-hash-table
    '()
    ))


(defmacro enqueue (xs x) `(setf ,xs (cons ,x ,xs)))

(defmacro dequeue (xs)
  `(let ((x (last ,xs)))
    (setf ,xs (butlast ,xs))))


(defun make-residual-network (flow-network)
  "return the residual network of a given flow network"
  )


