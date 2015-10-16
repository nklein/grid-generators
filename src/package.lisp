;;;; package.lisp

(defpackage #:grid-generators
  (:use #:cl #:list-types)
  (:export #:make-grid-generator)
  (:export #:taxicab-distance
           #:make-taxicab-generator)
  (:export #:make-lattice-generator)
  (:documentation "This package provides functions useful for
generating the points in grids."))
