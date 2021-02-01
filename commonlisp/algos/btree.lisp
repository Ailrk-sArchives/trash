;; binary tree implementation
(deftype uint () '(unsigned-byte 62))   ; define a integer alias.

(defconstant min-depth 4 "Minial depth of the binary tree.")
(defconstant num-workers 4 "Number of concurrent workers.")

(defun build-tree (depth)
  "Build binary tree with the given depth. Leaves are nil, branchs
   are cons cells"
  (declare (ftype (function (uint) list) build-tree)    ; declare a function type
           (uint depth)
           (optimize (speed 3) (safety 0)))
  (if (zerop depth)
      (cons nil nil)    ; why is this a tree?
      (cons (build-tree (1- depth))
            (build-tree (1- depth)))))

(defun check-node (node)
  (declare (ftype (function (list) uint) check-node)
           (optimize (speed 3) (safety 0)))
  (if (null (car node))
      1
      (the uint (+ 1 (check-node (car node)) (check-node (cdr node))))))

(defun check-trees-of-depth (depth max-depth)
  (declare (uint depth max-depth)
           (optimize (speed 3) (safety 0))))
