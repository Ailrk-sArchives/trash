;; use macros to generate functions

(defmacro gen-foo (v)
  (let ((fname (concatenate 'string
                            "foo-"
                            (symbol-name v)))
        (format t "~a" fname)
        `(defun ,fname () ,v))))

