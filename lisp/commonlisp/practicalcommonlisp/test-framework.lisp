;; chapter 9 test framework

(defvar *test-name* nil)

(defmacro deftest-unit (name parameters &body body)
  `(deftest ,name ,parameters
     (check ,@body)))

(defmacro deftest-suit (name parameters &body body)
  `(deftest ,name ,parameters
     (combine-results ,@body)))

(defmacro deftest (name parameters &body body)
  "define a test function"
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(defmacro check (&body test-cases)
  "run each test cases"
  `(combine-results
     ,@(loop for f in test-cases collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "combine the results of evaluating forms in order"
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect
               `(unless ,f (setf ,result nil)))
       ,result)))

(defun report-result (result form)
  "report the result of a single test case called by check"
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%"
          result *test-name* form) result)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym))) ,@body))


(deftest-suit test-arithmetics ()
              (test-arithmetics--/)
              (test-arithmetics-+*))


(deftest-suit test-arithmetics--/ ()
  (test--)
  (test-/))

(deftest-suit test-arithmetics-+* ()
  (test-*)
  (test-+))

(deftest-unit test-* ()
  (= (* 2 10) 20)
  (= (* 200 300) 60000))

(deftest-unit test-+ ()
  (= (+ 1 2) 3)
  (= (+ 1 2 3) 6)
  (= (+ 19 20) 38))

(deftest test-- ()
  (check
    (= (- 1 2) -1)
    (= (- 30 2 2) 27)
    (= (- 30 2 2) 26)))

(deftest test-/ ()
  (check
    (= (/ 10 2) 5)
    (= (/ 20 4) 5)))
