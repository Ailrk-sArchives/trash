(defclass kombucha ()
  ((flavor
     :initarg :flavor
     :accessor flavor
     :initform nil)))

(defclass monkey ()
  ((name
     :initarg :name
     :accessor monkey
     :initform nil)))


(defclass person ()
  ((name
     :initarg :name
     :accessor name
     :initform nil)))

(defclass child (person) ())

(defclass dog ()
  ((name
     :initarg :name
     :accessor name
     :initform nil)))

(defclass zaku ()
  ((id
     :initarg :id
     :accessor id
     :initform nil)))

(setf p1 (make-instance 'person :name "me"))
(setf p2 (make-instance 'child :name "alice"))
(setf k (make-instance 'kombucha :flavor "ginger"))
(setf m (make-instance 'monkey :name "mm"))
(setf d (make-instance 'dog :name "bob"))
(setf z1 (make-instance 'zaku :id "0x001"))


;; using generic functions

(defgeneric greet (obj)
  (:documentation "say hello")
  (:method (obj)
   (format t "Hi I am ~a" (type-of obj)))
  (:method :before ((obj person))
   (format t "Before person +++ ~%"))
  (:method ((obj person))
   (format t "Hi I am person, my name is ~a" (name obj))))



;; specializers
(defgeneric feed (obj meal-type)
  (:method (obj meal-type)
   (declare (ignorable meal-type))
   (format t "eating ~&")))

;; eql specializer
(defmethod feed (obj (meal-type (eql :dessert)))
  (declare (ignorable meal-type))
  (format t "mmh, dessert! ~&"))

;; a varaible and a eql specializer
(defmethod feed ((obj child) (meal-type (eql :soup)))
  (declare (ignorable meal-type))
  (format t "yammy Mom I like soup"))
