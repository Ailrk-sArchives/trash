;; dijkstra works with the shortest path.
;; O(VlogV + E) // dominated by E in lots of cases.


;; min heap
(defclass minheap ()
  ((data
     :type list
     :initarg :data
     :accessor data
     :initform (make-array 256 :adjustable t :fill-pointer 0))))


(defmethod print-object ((obj minheap) stream)
  "print the minheap"
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
  (if (= (mod i 2) 0)
      (/ (- i 2) 2)
      (/ (- i 1) 2)) )

(defmethod is-empty ((o minheap)) (= (length (data o)) 0))

(defmethod swimup ((o minheap))
  (format t "swimup ==>"))

(defmethod sinkdown ((o minheap))
  (format t "sinkdown"))

(defmethod insert-heap ((o minheap) e)
  "insert into the bottom of the heap then swimup"
  (with-accessors ((data data)) o
    (let ((was-empty (is-empty o)))
      (vector-push-extend e data)
      (if (not was-empty) (swimup o)))))

(defmethod extract-heap ((o minheap))
  "extract the min element, move bottom to top and sinkdown"
  (format t "extract")
  (with-accessors ((data data)) o
    (let ((top (elt data 0))
          (bottom (vector-pop data)))
      (setf (elt data 0) bottom)
      (sinkdown o 0)
      top)))

(defmethod search-heap ((o minheap))
  (format t "search"))


(defparameter *m* (make-instance 'minheap))

(defun dijkstra (graph)
  "shortest path")
