;;;; grid-iterate.asd

(asdf:defsystem #:grid-iterate
  :description "GRID-ITERATE provides functions useful for iterating
the points in grids using ITERATE."
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.4.20151016"
  :license "UNLICENSE"
  :depends-on (#:alexandria #:iterate (:version #:grid-generators
                                                "0.4.20151016"))
  :in-order-to ((asdf:test-op (asdf:load-op :grid-iterate-tests)))
  :perform (asdf:test-op (o c)
             (uiop:symbol-call :grid-iterate-tests :run-all-tests))
  :components
  ((:static-file "README.md")
   (:module "src"
    :components
    ((:module "iterate"
      :components ((:file "package")
                   (:file "cuboid" :depends-on ("package"))
                   (:file "taxicab" :depends-on ("package"))
                   (:file "lattice" :depends-on ("package"))))))))

(asdf:defsystem #:grid-iterate-tests
  :description "Tests for the GRID-ITERATE package."
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.4.20151016"
  :license "UNLICENSE"
  :depends-on ((:version #:grid-iterate "0.4.20151016") #:nst)
  :components
  ((:module "test"
    :components
    ((:module "iterate"
      :components ((:file "package")
                   (:file "criterion" :depends-on ("package"))
                   (:file "cuboid" :depends-on ("package"
                                                "criterion"))
                   (:file "taxicab" :depends-on ("package"
                                                 "criterion"))
                   (:file "lattice" :depends-on ("package"
                                                 "criterion"))
                   (:file "run" :depends-on ("package"
                                             "cuboid"
                                             "taxicab"
                                             "lattice"))))))))
