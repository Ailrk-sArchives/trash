;;;; CLOS

;;; common lisp object orientation system

(defvar *account-number* 0)

(defclass bank-account ()
  ((customer-name
     :initarg :customer-name)
   (balance
     :initarg :balance
     :initform 0)
   (account-number
     :initform (incf *account-number*))
   ; gold, silver, or bronze
   account-type))

(defclass checking-account (bank-account) ())

; define the :after method for initialize-instance.
; initialize instance is what's going to be called once you call
; make-instance.
; &key is required because that's how initialize-instance defined

(defmethod initialize-instance :after ((account bank-account)
    (setf (slot-value account 'account-type)
          (cond
            ((>= balance 10000) :gold)
            ((>= balance 1000) :silver)
            (t :bronze)))
    (when opening-bonus-percentage
      (incf balance (* balance (/ opening-bonus-percentage 100))))))

; mannually init
(defparameter *account* (make-instance 'bank-account))
(setf (slot-value *account* 'customer-name) "John Doe")
(setf (slot-value *account* 'balance) 1001)

; init with initalizer
(defparameter *president-account*
  (make-instance 'bank-account
                 :customer-name "Mr. President"
                 :balance 1000000
                 :opening-bonus-percentage 10))     ;; add some bonus

;; The core of CLOS: generic functions!
(defgeneric withdraw (account amount)
  (:documentation "Withdraw amount from the account"))

;; methods must have the same signatures as the generic function.
;; so &key, &rest also must be specified.
(defmethod withdraw ((account bank-account) amount)
  (let ((balance (slot-value account 'balance)))
    (when (< balance amount)
      (error "Account overdrawn."))
    (decf balance amount)))
