; read time conditionalizatio

(defun foo ()
  #+allegro (print "allegro")
  #+sbcl (print "sbcl")
  #+clisp (print "clisp")
  #+cmu (print "cmu")
  #- (or allegro sbcl clisp cmu) (error "not implemented"))

(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

(defun directory-path-p (p)
  (and
    (not (component-present-p (pathname-name p)))
    (not (component-present-p (pathname-name p)))
    p))

(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wile-pathname-p pathname)
      (error "Can't reliably convert wild pathnames"))
    (if (not (directory-path-p name))
        (make-pathname
          :directory (append (or (pathname-directory pathname)
                                 (list :relative))
                             (list (file-namestring pathname)))
          :name nil
          :type nil
          :defaults pathname)
        pathname)))
