;;;; Two commonly used assoc list.
;; plist is simply key value alterantivly stored in the list.
;; alist is a list of key value pairs.

;;;; Some examples with plist.
(setf plist '(:foo 10 :bar 20 :goo 30))
;; get
(getf plist :foo)
;; set
(setf (getf plist :foo) 11)
;; notice each symbol also has it's own plist
(get 'plist :k)
(setf (get 'plist :k) 100)

;; this plist is different from the plist we created above.
;; plist is a symbol that it's value is a property list
;; a symbol can have mutiple cells, refer to values in different name spaces.
;; the most famous example is value and function. ;
;; while (symbol-plist) is the plist in the symbol's property list cell.
;; plist historically was like slot in python that holds entries like value or
;; functions, but now it's just an annoying historical legacy.
(symbol-plist 'plist)   ;;  don't use it.

;; that's it...

;;;; Some examples with alist.
(setf alist '((:foo . 10) (:bar . 20) (:coo . 30)))

;; get from alist
(assoc :foo alist)

;; find
(rassoc 20 alist)
(rassoc-if (lambda (x) (> x 24)) alist)

;; push
(acons :doo 100 alist)

;; add buch of duplicates
(push  '(:poo . 99) alist)
(push  '(:poo . 99) alist)
(push  '(:poo . 99) alist)
(push  '(:poo . 99) alist)

(remove-duplicates alist :test #'equal)
