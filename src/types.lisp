;;;; types.lisp

(in-package #:grid-generators)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun list-of-reals-p (l)
    (every #'realp l)))

(deftype list-of-reals ()
  '(and list (satisfies list-of-reals-p)))
