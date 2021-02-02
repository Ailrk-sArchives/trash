;; Format is no longer a format function. It's more like a versatile type
;; converter in common lisp...
;; format has three main functionalities
;; 1. printing tables of data.
;; 2. pretty printing s expressions.
;; 3. print values in human readable form.

;; the first argument of format is a output stream. It can be
;; nil, t, *standard-output*, or a string with filled pointer.
;; with a string with a filled pointer, you can get the result of
;; format as a variable, much like sprintf in c.
(format t "~{~a^, ~}" '(1 2 3 4 5 6 7 8 9 10))

(format t "~a" pi)

(format t "~$" pi)  ;; floating point with 2 digitis
(format t "~5$" pi)

;; prefix parameter allows you to alter some behaviors
(format t "~v$" 3 pi) ;; v consume one format paramter, use it to set prefix.
(format t "~3$" pi)   ;; this is the same as the line above.

(format t "~#$" pi)  ;; # will evaluated as # of remaing format arguments
(format t "~#$" pi 1 2 3)


;; an example of outputing formatted string to fillpointer string.
(let ((s1 (make-array 0 :element-type 'character
                      :adjustable t
                      :fill-pointer 0))  ;; fill at the beginning
      (s2 (make-array 10 :element-type 'character
                      :adjustable t
                      :fill-pointer 4))
      )
     (format s1 "Hello, ~a" 'Jimmy)
     (format s2 "Hello, ~a" 'Jimmy)
     (print s1)
     (print s2)
     nil)
