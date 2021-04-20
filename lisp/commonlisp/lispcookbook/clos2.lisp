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

;; define a method
;; if a method has no generic function, cl will automatically add
;; a definition for the generic function.

(defmethod greet (obj)
  (format t "Hi I am a ~a" (type-of obj)))

(defmethod greet ((obj dog))
  (format t "Wof I am a dog, my name is ~a" (name obj)))

(defmethod greet :before ((obj person))
  (format t "++++++ before person +++++~%"))

(defmethod greet ((obj person))
  (format t "I am a person and my name is ~a" (name obj)))

(defmethod greet :after ((obj child))
  (format t "~%++++++ after child +++++~%")
  (format t "... ~a is clawing away" (name obj)) )

(defmethod greet ((obj zaku))
  (format t "Zig Zeon! I am zaku number ~a" (id obj)))

(defmethod greet ((obj kombucha))
  (with-accessors ((flavor flavor))
    obj
    (format t "Hi I am a can of kombucha, my flavor is ~a" flavor)))
