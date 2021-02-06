(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number)
          never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms
                       for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names
                         for g in gensyms collect `(,n ,g)))
             ,@body)))))

(defmacro do-primes ((var start end) &body body)
  (once-only (start end)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
     ((> ,var ,end))
     ,@body)))

;;;; how to write a hyneic macro writing macro?
(defmacro repeat-times-writing-robot (count-form forms)
  (let ((counter-sym-sym (gensym)))  ; macro's symbol
    `(let ((,counter-sym-sym (gensym)))
       ;; unquote twice for the outer most symbol
       `(loop for ,,counter-sym-sym below ,,count-form
              do ,@,forms))))

(defmacro repeat-times (count-form &body forms)
  (repeat-times-writing-robot count-form forms))
