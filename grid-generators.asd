;;;; grid-generators.asd

(asdf:defsystem #:grid-generators
  :description "GRID-GENERATORS is a package of functions useful for generating the points in a grid."
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.2.20151009"
  :license "UNLICENSE"
  :depends-on (#:alexandria #:let-plus #:list-types)
  :in-order-to ((asdf:test-op (asdf:load-op :grid-generators-tests)))
  :perform (asdf:test-op (o c)
             (uiop:symbol-call :grid-generators-tests :run-all-tests))
  :components
  ((:static-file "README.md")
   (:module "src"
    :components ((:file "package")
                 (:file "cuboid" :depends-on ("package"))
                 (:file "taxicab" :depends-on ("package"))))))

(asdf:defsystem #:grid-generators-tests
  :description "Tests for the GRID-GENERATORS package."
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.2.20151009"
  :license "UNLICENSE"
  :depends-on (#:grid-generators #:nst)
  :components
  ((:module "test"
    :components ((:file "package")
                 (:file "criterion" :depends-on ("package"))
                 (:file "cuboid" :depends-on ("package"
                                              "criterion"))
                 (:file "taxicab" :depends-on ("package"
                                               "criterion"))
                 (:file "run" :depends-on ("package"))))))
