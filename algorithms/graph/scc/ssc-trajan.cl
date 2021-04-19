;; trajan ssc only need one dfs

(defmacro init-hash-table (xs)
  `(let ((m (make-hash-table)))
     (loop for kv in ,xs do
           (setf (gethash (car kv) m) (cdr kv)))
     m))


;; there are three strongly connected components here.
;; (a b e), (c d h) (f g)
(defparameter *graph*
  (init-hash-table
    '((a . (b))
      (b . (e f c))
      (c . (d g))
      (d . (c h))
      (e . (a f))
      (f . (g))
      (g . (f))
      (h . (g d)))))

(defparameter *graph-1*
  (init-hash-table
    '((3 . (0))
      (0 . (1))
      (1 . (2))
      (2 . (3 4))
      (4 . (5))
      (5 . (6))
      (6 . (4))
      (7 . (8 6))
      (8 . nil))))


