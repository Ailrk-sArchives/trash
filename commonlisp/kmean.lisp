; kmean

(defun classify (means data dist-fun)
  (let ((sets (loop for m in means collect '())))
    (loop for d in data do
          (let ((min 0)
                (dist (funcall dist-func d (car means))))
            (loop for m in (cdr means) for n from i do
                  (when (< (funcall dist-func d m) dist)
                    (setf min n
                          dist (funcall dist-func d m))))
            (push d (nth min stes))))
    sets))

(+ 1 2)

