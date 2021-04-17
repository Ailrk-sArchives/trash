;; find topological order of a directed acyclic graph (DAG problem)

(defmacro init-hash-table (xs)
  `(let ((m (make-hash-table)))
     (loop for kv in ,xs do
         (setf (gethash (car kv) m) (cdr kv)))
     m))


;; topological sort works on a directed acyclic graph.
(defparameter *dependencies*
  (init-hash-table '((tar . nil)
                     (coreutils . (libbz2 libselinux1))
                     (dpkg . (coreutils tar multiarch-support))
                     (multiarch-support . nil)
                     (libselinux1 . (multiarch-support)))))

(defun dfs (graph root)
  (let* ((visited `(,root))
         (stack `(,root)))
    (loop while stack do
          (let ((v (pop stack)))
            (loop for u in (gethash v graph) do
                  (if (not (member u visited))
                      (progn
                        (pushnew u visited)
                        (push u stack))))))
    (reverse visited)))

; (format t "~a~%" (dfs *dependencies* 'dpkg))
; (format t "~a~%" (dfs *dependencies* 'libselinux1))


