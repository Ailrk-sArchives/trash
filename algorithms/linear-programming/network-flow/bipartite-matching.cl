;; maximum bipartite matching problem.

;; A match in a bipartite graph is a set of edges chosen
;; in such a way that no two edges share and endpoint.

;; A maximum matching is a match of maximum size.

;;;; MBP (maximum bipartite mactch) and network flow.
;; the problem can be solve by finding the maximum flow in a flow
;; network.

;; A bipartite graph can be thought as a flow network with multiple
;; sinks and sources.
;; We can add super sink and super source to turn the problem into a
;; single sink single source problem.


;; steps
;; 1. Convert the bipartite graph into a single source single sink
;;    flow network.
;; 2. Find the maximum flow with ford-fulkerson.
;;    The maximum flow is the bipartite matching we are looking for.


(defmacro init-hash-table (xs)
  `(let ((m (make-hash-table)))
     (dolist (kv ,xs)
       (setf (gethash (car kv) m) (cdr kv)))
     m))


(defparameter *bipartite-graph*
  (init-hash-table
    '((a . (2 3))
      (b . nil)
      (c . (1 4))
      (d . (3))
      (e . (3 4))
      (f . (6))
      (nil . (5)))))


(defun make-network (bipartite)
  "first make the binary matching network
   note the original bipartite graph doesn't have capacity,
   but they have implicit 1. Because you can either connect to
   another node or not.

   A flow network contains (u f . c)
   where u is the node name, f is the current flow, and c is the
   capacity.
   "
  (let ((left nil)
        (right nil)
        (s* nil)
        (t* nil))
    (maphash
      (lambda (k v)
        (push k left)
        (setf right (remove-duplicates (append v right))))
      bipartite)
    (setf left (remove nil left) right (remove nil right))
    (dolist (n left) (push n s*))
    (dolist (n right) (push n t*))
    (setf (gethash 's bipartite) s*
          (gethash 't bipartite) t*)
    (loop :for k :being :the :hash-keys :in bipartite :using (hash-value v)
          :do
          (setf (gethash k bipartite)
                (mapcar (lambda (x) (cons x (cons 0 1))) v)))
    bipartite))


;; residual network
;;
;; cf(u, v) = {
;;    c(u, v) - f(u, v)  if (u, v) in E
;;    c(v, u)            if (v, u) in E     unsend
;;    0                  otherwise
;; }


(defun make-residual-network (graph)
  "make the residual network"
  )

(defun augmenting-path (residual-network)
  "finding the augmenting path"

  )

(defun edmonds-karp (graph)
  "run edmonds karp (ford fulkerson)"

  )


(defun print-hash (m)
  (maphash (lambda (k v)
             (progn
               (write (list k v))
               (format t "~%")))
           m))


