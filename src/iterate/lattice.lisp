;;;; lattice.lisp

(in-package #:grid-iterate)

(iterate:defmacro-driver (iterate:FOR var
                                      BY-LATTICE-STEPS basis-vectors
                          &optional MINIMUM-STEPS min-steps
                                    MAXIMUM-STEPS max-steps
                                    WITH-SCALE scale
                                    WITH-OFFSET offset)
  (alexandria:with-gensyms (generator new got)
    (let ((kwd (if iterate:generate
                   'iterate:generate
                   'iterate:for)))
      `(progn
         (iterate:with ,generator
                       = (grid-generators:make-lattice-generator
                                 ,basis-vectors
                                 ,@(when min-steps `(:minimum-steps ,min-steps))
                                 ,@(when max-steps `(:maximum-steps ,max-steps))
                                 ,@(when scale `(:scale ,scale))
                                 ,@(when offset `(:offset ,offset))))
         (,kwd ,var next (multiple-value-bind (,new ,got)
                             (funcall ,generator)
                           (if ,got
                               ,new
                               (iterate:terminate))))))))
