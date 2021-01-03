;; 10 numbers - 11 collections - 12 list - 13 trees

;; lisp is surprisingly rich with it's data structures support.
;; lots of things are in the standard library direclty,
;; not like in haskell, to use vector you need extra dependency.


;; vectors

(defun play-with-vector ()
  (let ((vec1 (vector 1 2 3))
        (vec2 #(13 2))
        (vec3 (make-array 5 :initial-element nil))
        ;; resizable vector. :adjustable is not required.
        (vec4 (make-array 5 :fill-pointer 3
                          :adjustable t)))
    (format t "vec4 before push: ~a~%" vec4)

    (vector-push 'a vec4)
    (format t "vec4 after push ~a~%" vec4)

    (vector-pop vec4)
    (format t "vec4 after pop ~a~%" vec4)

    (loop for n in '(1 2 3 4) do (vector-push-extend n vec4))
    (format t "vec4 after grown ~a~%" vec4)))

(defun specialized-vector ()
  (let ((char-vec (make-array 5 :fill-pointer 4
                              :adjustable t
                              :initial-element #\Z
                              :element-type 'character))
        (bit-vec (make-array 5 :fill-pointer 0
                             :adjustable t
                             :element-type 'bit)))
    ;; length and elt are defined for all sequences.
    (format t "~a ~%"(length char-vec))
    (setf (elt char-vec 2) #\X)

    (vector-push #\A char-vec)
    (format t "before set elt : ~a~%" char-vec)

    (setf (elt char-vec 0) #\B)
    (format t "after set elt : ~a~%" char-vec)
    ))

(defmacro show (clause)
  (let ((name (gensym)))
    `(let ((,name ',clause))
       (format t "[code]: ~a [result]: ~a~%" ,name ,clause))))

(defun verbose-first (x)
  (format t "Looking at ~s~%" x) (first x))

(defun sequence-iterating ()
  (let ((seq1 (make-array 5 :fill-pointer 5
                           :adjustable t
                           :element-type 'number
                           :initial-element 1
                           )))
    (show (count 1 #(1 2 3 1 1)))
    (show (remove 1 #(1 2 3 1 1)))
    (show (remove 1 '(1 2 3 1 1)))
    (show (remove #\A "abcAbcA")) (show (substitute 9 1 #(1 2 3 1 1)))
    (show (find 'a #((a 10) (b 20) (c 30)) :key #'first))
    (show (remove #\a "foobarbazbarbar" :count 2))
    (show (count 'a #((a 10) (b 20) (c 30)) :key #'verbose-first))
    (show (remove #\a "foobarbar" :count 1 :from-end t))
    (show (find 3 #(1 2 3 1 1)))))


(defun sequence-function-variants ()
  (show (remove-if-not #'evenp (loop for n below 10 collect n)))
  (show (count-if #'oddp (loop for n below 10 by 2 collect n))))


(defun some-operations ()
    (concatenate 'vector #(1 2 3) '(4 5 6))
    (concatenate 'list '(1 2 3) #(3 4 5))
    (concatenate 'string "abc" '(#\a #\b))
    (sort #("asd" "qd" "bvkds") #'string>)
    (let ((my-seq #(9 3 4 2 7 1)))
      (setf my-seq (sort my-seq #'<))   ; sort is destructive update
      (elt my-seq 0))
    (merge 'vector #(1 3 5) #(2 4 6) #'<)
    (subseq "foobarbaz" 3)
    (subseq "foobarbaz" 3 6)
    (let ((my-seq #(9 8 3 4)))      ; subseq is setable.
      (setf (subseq my-seq 2 4) #(1 2))
      (write my-seq))
    (every #'evenp #(1 2 3 4 5))
    (some #'evenp #(1 2 3 4 5))
    (map 'vector #'* #(1 2 3 4) #(1 2 3 4))
    (map 'vector (lambda (x) (* x 10)) #(1 2 3 4))
    (reduce #'+ (loop for n below 11 collect n) :initial-value 0)
    (reduce #'- (loop for n below 11 collect n) :initial-value 0)
    (reduce #'- (loop for n below 11 collect n)
            :initial-value 0 :from-end t))

(defmacro test-hash (fn)
  (let ((hash-table-sym (gensym)))
    `(let ((,hash-table-sym (make-hash-table)))
       (,fn ,hash-table-sym))))

; play with multiple return values.
(defun show-value (key hash-table)
  (multiple-value-bind (value present) (gethash key hash-table)
    (if present
        (format t "Value ~a by ~a actually present.~%" value key)
        (format t "Value ~a by ~a doesn't present.~%" value key))))

(defun play-hash ()
  (test-hash (lambda (table) (gethash 'foo table)))
  (test-hash (lambda (table)
               (setf (gethash 'foo table) 1)
               (gethash 'foo table)))

  (test-hash (lambda (table)
               (setf (gethash 'foo table) nil)
               (show-value 'foo table)
               (show-value 'bar table)
               (remhash 'foo table)     ; remove element
               (show-value 'foo table)
               ))

  (test-hash (lambda (table)
               (setf (gethash 'foo table) 1)
               (setf (gethash 'bar table) 2)
               (setf (gethash 'bz table) 2)
               (maphash (lambda (k v)
                          (format t "key ~a, value: ~a~%" k v))
                        table)
               (loop for k being the hash-keys
                     in table using (hash-value v)
                     do (format t "~a => ~a~%" k v))
               )))

; cons cell: (car, cdr), which forms a pair.

(defun car-cdr-cons ()
  (cons 1 2) ; => (1 . 2) dotted pair
  (cons 1 (cons 2 nil))
  (cons 1 (cons 2 (cons 3 nil))) ; (1.)->(2.)->(3.nil)
  (list "foo" (list 1 2) 10)

  (append (list 1 2 3) (list 3 4 5))
  (reverse (list 1 2 3))

  ; for side effects / recycling
  (let ((list1 (list 1 2 3)))   ; destructive

    ; use side effects, be careful to mix it with
    ; functional style code.
    (setf (first list1) 3)
    (format t "~a~%" list1)

    (setf (elt list1 2) 99)
    (format t "~a~%" list1)

    (setf (aref list1 3)))
  ; recycling, use side effect to optmize functional
  ; code
  ; recycling functions can only be used safely
  ; if the list isn't gonna be used afterwards.

  (let ((list1 (list 1 2 3)))

    (reverse list1)
    (format t "~a~%" list1)

    (setf list1 (reverse list1))
    (format t "~a~%" list1)

    (nreverse list1))

  (first (list 1 2 3))
  (rest (list 1 2 3))
  (last (list 1 2 3))
  (mapcar (lambda (x) (+ x 1)) (list 1 2 3))
  (nth 3 (list 1 2 3 4))
  (quote ((1 2) (3 4) (5 6)))

  (let* ((list1 '(1 2 3))
         (list2 (copy-list list1))
         (list3 '((1 2) (3 4)))
         (list4 (copy-list list3))
         )
    (setf (elt list2 2) 99)

    (format t "list1: ~a~%" list1) ; (1 2 3)
    (format t "list2: ~a~%" list2) ; (1 2 99)

    (setf (elt list3 1) 99)

    (format t "list3: ~a~%" list3)
    (format t "list4: ~a~%" list4)
    (tree-equal list1 list2 :test #'equal))

  ; tree with con cells
  (let ((tree1 '(1 2 (3 2 1) (1 1) (2 2))))
    (subst 10 1 tree1)
    (subst-if 10 #'listp tree1)) ; TODO works

  ; set with con cells
  (let ((s '()))
    (pushnew 1 s)   ; destructive
    (adjoin 2 s)))

(defun assoc-list ()
  ; lookup table
  ; association list (alist) and property list (plist)

  ; assoc list is just list with dotted pairs as it's elements.
  '((a . 1) (b . 2) (c . 3))

  ; use assoc to look up
  (assoc 'a '((a . 1) (b . 2) (c . 3)))
  (cdr (assoc 'a '((a . 1) (b . 2) (c . 3))))
  (cdr (assoc "a" '(("a" . 1) ("b" . 2) ("c" . 3)) :test #'string=))

  ; add an key-value pair to the list.
  (cons (cons 'd 3) '((a . 1) (b . 2) (c . 3)))
  ; short hand
  (acons 'd 3 '((a . 1) (b . 2) (c . 3)))

  ;;; for small table and the performance of alist can
  ;;; out perform hashtable, since hashtable itself has
  ;;; some extra overhead like hashing and collision resolving.
  ;;; There are a lot of situations that you only need a small
  ;;; table. For example, for the entire problem you only have
  ;;; 20 or 30 special configs to keep track of. alist can be
  ;;; a very good candidate for that.

 (let* ((alist (acons 1 "one" (acons 2 "two" '())))
        (list-copy (copy-list alist))
        (alist-copy (copy-alist alist))
        (show (lambda ()
                (format t "alist      ~a ~%" alist)
                (format t "list-copy  ~a ~%" list-copy)
                (format t "alist-copy ~a ~%" alist-copy)
                (format t "~%")
                )))
   ; conclusion list-copy share the same underlying atoms.
   ; alist copy copy the entire table
   (funcall show)

   (setf (cdr (assoc 2 alist)) "deux")
   (funcall show)

   (setf (cdr (assoc 1 list-copy)) "uno")
   (funcall show)

   (setf (cdr (assoc 1 alist-copy)) "yi")
   (funcall show))

 ; plist is similar but use symbols directly

 (let ((plist '(:a 1 :b 2 :c 3)))
   (getf plist :a)
   (setf (getf plist :a) 10)
   (remf plist :a)
   (write plist))


 ; destructruring binding
 (destructuring-bind (x y z) (list 1 2 3)
   (list :x x :y y :z z))

 (destructuring-bind (x y z) (list 1 (list 1 2) 3)
   (list :x x :y y :z z))

 (destructuring-bind (&whole whole &key x y z)
   (list :z 1 :x 1 :y 1)
   (write whole)))
