;;;; package.lisp

(defpackage #:grid-generators
  (:use #:cl)
  (:export #:make-grid-generator)
  (:export #:taxicab-distance
           #:make-taxicab-generator)
  (:documentation "This package provides functions useful for
generating the points in grids."))
