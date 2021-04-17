;; yet another version in common lisp

(defparameter *graph-str*
  "
  0 - 1 - 3
  | / |
  2 - 5 - 7 - 8 - 9
  |   |   |
  4   6 - 10 - 11 - 12 - 13 - 14 - 15
  ")

(defparameter *graph*
  '((1) (0 2 3) (1 3 4 5)  ;; 0 - 2
        (1 2 5) (2) (2 3 6 7)  ;; 3 - 5
        (5 10) (5 8 10) (7 9) (8)  ;; 6 - 9
        (6 7 11) (10 12) (11 13)  ;; 10 - 12
        (12 14) (13 15) (14)         ;; 13 - 15
        ))

(defun dfs (graph root)
  (let* ((visited `(,root))
         (stack `(,root)))
    (loop while stack do
          (let ((v (pop stack)))
            (loop for u in (elt graph v) do
                  (if (not (member u visited))
                      (progn
                        (setf visited (append visited `(,u)))
                        (push u stack))))))
    visited))

;; to mutate outside world you need to use macro.
(defmacro dequeue (queue)
  (let ((v (gensym)))
    `(let ((,v (car (last ,queue))))
       (setf ,queue (butlast ,queue))
       ,v)))

(defmacro enqueue (queue x)
  `(setf ,queue (cons ,x ,queue)))

(defun bfs (graph root)
  (let* ((visited `(,root))
         (queue `(,root)))
    (loop while queue do
          (let ((v (dequeue queue)))
            (loop for u in (elt graph v) do
                  (if (not (member u visited))
                      (progn
                        (setf visited (append visited `(,u)))
                        (enqueue queue u))))))
    visited))


(format t "~%dfs: ~a~%" (dfs *graph* 0))
(format t "~%bfs: ~a~%" (bfs *graph* 0))
