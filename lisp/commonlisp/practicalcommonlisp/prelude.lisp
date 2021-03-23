;;; early chapters

(defun make-row (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

;;; *xx* is the name convention for global variables
(defvar *db* nil)
(setf *db* nil)

(defun add-record (row) (push row *db*))

;;; format: ~a  :  aesthetic directive
;;;         ~10t:  10 space
;;;         ~%  :  emit 1 new line
;;;         ~{ }~ : process format string multiple times
(defun dump-record ()
  (dolist (row *db*)
    (format t "~5tCD Records~%~{~a:~10t~a~%~}~%" row)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*) ;
  (read-line *query-io*))

(defun prompt-for-row ()
  (make-row
    (prompt-read "Title")
    (prompt-read "Artist")
    (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
    (y-or-n-p "Ripped")))

(add-record (make-row "sunny" "potsu" 100 t))
(add-record (make-row "banana with nuts" "potsu" 100 nil))
(add-record (make-row "bossa uh" "potsu" 100 t))
(add-record (make-row "misty" "ella fizgerald" 100 nil))
(add-record (make-row "ain't misbehavin" "joe pass" 100 nil))

(defun add-rows ()
  (loop (add-record (prompt-for-row))
        (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename
                      :direction :input)     ; default is reading
    (with-standard-io-syntax
      (setf *db* (read in)))))

;;; keyword parameter and supplied-p
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

;;; to build a list pass symbols, this stop lisp from evaluating.
(defun make-comparison-expr-old (field value)
  (list 'equal (list 'getf 'row field) value))

;;; with quasi quotes
(defun make-row-expr (op row field value)
  `(,op (getf ,row ,field) ,value))

(defun make-comparison-expr (field value)
  (make-row-expr 'equal 'row field value))

(defun make-comparison-list (records)
  (loop while records
        collecting (make-comparison-expr (pop records) (pop records))))

;;; &rest makes the function varadic
(defmacro where (&rest clauses)
  `(lambda (row) (and ,@(make-comparison-list clauses))))

(defun make-update-expr (field value)
  (make-row-expr 'setf 'row field value))

(defun make-update-expr-list (records)
  (loop while records
        collecting (make-update-expr (pop records) (pop records))))

;;; (update (where :title "misty") :title "Misty" :rating 101)
(defmacro update (selector-fn &rest clauses)
  `(setf *db* (mapcar
                (lambda (row)
                  (when (funcall ,selector-fn row)
                    ,@(make-update-expr-list clauses)) row)
                *db*)))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))
