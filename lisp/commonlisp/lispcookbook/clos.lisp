;; it takes some practice.
;; you can't necessarily remember all usage without usig them...

;; idea of clos:
;; 1. class are just data with some accessors
;; 2. polymorphism is achieved by dispatch generic functions
;; 3. generic functions has specific implemnetations called mathods.
;; 4. once a generic function called, it will stich multiple method implementations
;;    to form a effective method.
;; 5. clos support multi dispath, meaning the decision of which method to call can be
;;    determined by more than one arguments. (in C++ dynamic dispatching only dispatch on
;;    one method, so it's single dispath)


;;;;
;; -- define a class
;; attributes of classes are called slots.
(defclass person ()
  ((name
     :initarg :name   ;; define init arg
     :accessor name)  ;; define the accessor name used
   (lisper
     :initarg :lisper
     :initform nil
     :accessor lisper)
   (species
     :initform 'homo-sapiens
     :accessor species
     :allocation :class)))
     ;; allocation control if the field is local or shared
     ;; between classes. By default is local (member)
     ;; set to :class to make it `static`.

;; -- create an object with make-instance
(defparameter *p1* (make-instance 'person :name "me" :lisper nil))

(defparameter *p2* (make-instance 'person :name "you" :lisper '(1 2 3)))

;; -- access slot with slot-value
;; slot is a function that always work...
(slot-value *p1* 'name)
(slot-value *p1* 'lisper)
(slot-value *p1* 'name)
(slot-value *p2* 'lisper)

(setf (slot-value *p1* 'lisper) '(-1))
(setf (slot-value *p1* 'lisper) '(9 8 7))

;; -- access slot predefined accessor
(name *p1*)
(name *p2*)
(funcall #'name *p1*)   ;; name is a funtion generated.

(class-of *p1*)
(type-of *p1*)
;; (inspect *p1*)

;; short hand for accessing necessary slots.
(with-slots ((n name)
             (l lisper))
  *p1*
  (format t "~a, ~A~%" n l))

;; -- set the shared field will change all instances.

;;;; inheritance

(defclass child (person)
  (can-walk-p
    :accessor can-walk-p
    :initform t))

(defmethod greet (obj)
  (format t "You are a ~a~&" (type-of obj)))
