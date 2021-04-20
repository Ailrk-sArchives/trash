;; dijkstra works with the shortest path.
;; O(VlogV + E) // dominated by E in lots of cases.
(defpackage #:dijkstra
  (:use "COMMON-LISP")
  (:shadow "+"))

(in-package #:dijkstra)

;; min heap
(defclass min-heap ()
  ((data
     :type list
     :initarg :data
     :accessor data
     :initform (make-array 256 :adjustable t :fill-pointer 0))))

(defmethod print-object ((obj min-heap) stream)
  "print the min-heap"
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((data data))
      obj
      (format stream "~%minheap data: ~a" data))))

(defun left-child (i)
  (declare (type integer i))
  (+ (* 2 i) 1))

(defun right-child (i)
  (declare (type integer i))
  (+ (* 2 i) 2))

(defun parent (i)
  (floor (cond ((= i 0) 0)
               ((= (mod i 2) 0) (/ (- i 2) 2))
               (t (/ (- i 1) 2)))))

(defmethod is-empty ((o min-heap)) (= (length (data o)) 0))

(defmethod swimup ((o min-heap) i)
  "i is the index of the element to swim up"
  (with-accessors ((data data)) o
    (if (< (elt data i) (elt data (parent i)))
        (progn
          (rotatef (elt data i) (elt data (parent i)))
          (swimup o (parent i))))))

(defmethod sinkdown ((o min-heap) i)
  "sink down while keep the order invariant"
  (with-accessors ((data data))
    o
    (macrolet ((elt-or-nil (seq i)
                 `(if (>= ,i (length ,seq)) nil (elt ,seq ,i))))
      (let* ((largest-idx i)
             (e (elt-or-nil data i))
             (left-idx (left-child i))
             (right-idx (right-child i))
             (left (elt-or-nil data left-idx))
             (right (elt-or-nil data right-idx)))
        (if e
          (progn
            (if (and left (> e left)) (setf largest-idx left-idx))
            (if (and right (> e right)) (setf largest-idx right-idx))
            (if (not (= largest-idx i))
                (progn
                  (rotatef (elt data largest-idx) (elt data i))
                  (sinkdown o i)))))))))

(defmethod insert-heap ((o min-heap) e)
  "insert into the bottom of the heap then swimup"
  (with-accessors ((data data)) o
    (let ((was-empty (is-empty o)))
      (vector-push-extend e data)
      (if (not was-empty) (swimup o (- (length data) 1))))))

(defmethod extract-heap ((o min-heap))
  "extract the min element, move bottom to top and sinkdown"
  (format t "extract")
  (with-accessors ((data data)) o
    (if (is-empty o)
        nil
        (let ((top (elt data 0))
              (bottom (vector-pop data)))
          (if (not (is-empty o))
              (setf (elt data 0) bottom))
          (sinkdown o 0)
          top))))

(defmethod search-heap ((o min-heap))
  (format t "search"))

;; test
; (defparameter *m* (make-instance 'min-heap))
; (let ((xs '(8 3 12 5 1)))
;   (loop for i in xs do
;         (insert-heap *m* i)))

;; overload the operator

(defun + (&rest addends)
  (reduce 'binary+ (cdr addends) :initial-value (car addends)))

(defgeneric binary+ (a b)
  (:documentation "overload add operator"))

(defmethod binary+ ((a number) (b number))
  (cl:+ a b))


(defclass node ()
  ((name
     :type symbol
     :initarg :name
     :accessor name
     :initform nil)
   (distance
     :type number
     :initarg :distance
     :accessor distance
     :initform most-positive-word)
   (predecessor
     :type node
     :initarg :predecessor
     :accessor predecessor
     :initform nil)))


; (defmacro relax (u v w)
;   "relax adjacent nodes"
;   `())

; (defun dijkstra (graph)
;   "shortest path"
;   (let ((mh (make-instance 'min-heap)))

;     ))
