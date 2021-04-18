;u some fishies
;; Some pair of fish fight if they are in the same tank.
;; put fish in two tanks so they don't fight.

;; use bfs for testing if a graph is bipartite.
;; coloring the graph layer by layer

;; why does this work?
;; if you can color each layers with different color, the graph
;; looks like a tree, you can move odd index layer to one side, even index
;; layers to another.

;; if you have an edge from an odd index and connecet to another odd index,
;; you have a cycle. What should you color the target node, odd color or
;; even color?

(defmacro init-hash-table (xs)
  `(let ((m (make-hash-table)))
     (loop for kv in ,xs do
           (setf (gethash (car kv) m) (cdr kv)))
     m))

(defmacro enqueue (x xs) `(push ,x ,xs))

(defmacro dequeue (xs)
  (let ((v (gensym)))
    `(let ((,v (car (last ,xs))))
       (setf ,xs (butlast ,xs))
       ,v)))

(defparameter *fish-fight-graph*
  (init-hash-table
    '((a . (b c))
      (b . (a d))
      (c . (a d))
      (d . (b c e f))
      (e . (d g h))
      (f . (d g h))
      (g . (f e))
      (h . (f e)))))

;; a simple assoc list
(defparameter *color-table*
  (let ((keys (remove-duplicates
    (append (loop for k being the hash-keys in *fish-fight-graph*)
            (apply #'append
                   (loop for v being the hash-values in *fish-fight-graph*
                         collect v)) ))))
    (mapcar (lambda (n) (cons n -1)) keys)))

(defun set-color (node color) (setf (cdr (assoc node *color-table*)) color))

(defun next-color (n) (mod n 2))

(defun bipartite (graph root)
  (let* ((queue `(,root))
         (visited `(,root))
         (color 0))
    (set-color v color)
    (loop while queue do
          (let ((v (dequeue queue)))
            (setf color (next-color color))
            (loop for u in (gethash v graph) do
                  (if (not (member u visited))
                      (append visited `(,u))
                      (if (not (equal (assoc u *color-table*) -1))
                          (error "found cycle")
                          (set-color u color))))))))
