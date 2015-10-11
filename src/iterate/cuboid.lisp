;;; cuboid.lisp

(in-package :grid-iterate)

(iterate:defmacro-driver (iterate:FOR var ON-GRID-TO to
                          &optional FROM from-var BY by-var)
  (alexandria:with-gensyms (generator new got)
    (let ((kwd (if iterate:generate
                   'iterate:generate
                   'iterate:for)))
      `(progn
         (iterate:with ,generator
                       = (grid-generators:make-grid-generator ,to
                                                              :from ,from-var
                                                              :by ,by-var))
         (,kwd ,var next (multiple-value-bind (,new ,got)
                             (funcall ,generator)
                           (if ,got
                               ,new
                               (iterate:terminate))))))))
