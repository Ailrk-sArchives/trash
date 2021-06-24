;;;; setf
;;; is a macro that sets a value to a place.

;; object: any lisp datum.
;; form: an object meant to be evaluated / a symbol
;; cell: a conceptual slot of an object. in lisp 2
;;       an object can have two cells: variable cell and
;;       function cell
;; slot: a component of an object that can store a value
;; symbol: used for objet identity
;; place: a form which is suitable for uses as a generalized reference
;;        / the conceptual location referred by a place
;; generalized reference: a reference to a location storing an object

(defmacro my-setf (place values-form &environment env)
  (multiple-value-bind (vars vals stores setter)
    (get-setf-expansion place env)
    `(let* ,(mapcar #'list vars vals)
       (multiple-value-bind ,stores ,values-form ,setter))))

(let ((list (list 1 2 3 4)))
  (my-setf (elt list 2) 100))

;; setq is a low level primitive for setting the value to a
;; symbol if a value is binded to that symbol.
(let ((a 10))
  (setq a 11)
  a)


;;; this doesn't work for setq
;;; (let ((a '(1 2 3)))
;;;   (setq (elt a 1) 20)
;;;   a)

;;; setf is a macro
;;; setf is designed for more generic set. it works for all setq
;;; cases, as they just get expanded to (setq a x)
;
;;; but you can define you own place with defsetf, meaning you can
;;; customize a place to be settable.

(defun eleventh (ls)
  (elt ls 10))

(defun set-eleventh (ls new-val)
  (setf (elt ls 10) new-val))

(defsetf eleventh set-eleventh)

(let ((l (loop for i from 0 to 15 collect i)))
  (setf (eleventh l) :foo)
  l)


;;; some experiments
(let ((a 10))
  (setf a 10)   ;; (setq a 10)
  (incf a)      ;; (setq a (+ 1 a))
  a)

(let ((a '(1 2 3)))
  (setf (elt a 2) 10) ;; (sb-kernel:%setelt a 2 10)
  a)

(let* ((l '(1 2 3))
       (a (elt l 1)))
  (setf a 10)
  (values a l))
