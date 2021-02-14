;; again, some basic stuffs.


; (defun destructive-rev (lst)
;   (let* ((len (length lst))
;          (ilimit (truncate (/ len 2))))
;     (do ((i 0 (1+ i))
;          (j (1- len) (1- j)))
;         (rotatef (nth i lst) (nth j lst)))))

;; reverse can be either destructive or non destructive.
(defun destructive-rev (lst)
  (let* ((len (length lst))
         (limit (truncate (/ len 2))))
    (do ((i 0 (1+ i))
         (j (- len 1) (1- j)))
        ((>= i limit))
        (rotatef (elt lst i) (elt lst j))))
  lst)

(defun immut-rev (lst)
  (labels ((rec (xs acc)
             (if (null xs)
                 acc
                 (rec (cdr xs) (cons (car xs) acc)))))
    (rec lst nil)))

;; ok that's all...
;; this chapter really doesn't have much
