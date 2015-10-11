;;;; grid-iterate.asd

(asdf:defsystem #:grid-iterate
  :description "GRID-ITERATE provides functions useful for iterating
the points in grids using ITERATE."
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20151011"
  :license "UNLICENSE"
  :depends-on (#:alexandria #:iterate #:grid-generators)
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
                   (:file "taxicab" :depends-on ("package"))))))))

(asdf:defsystem #:grid-iterate-tests
  :description "Tests for the GRID-ITERATE package."
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20151011"
  :license "UNLICENSE"
  :depends-on (#:grid-iterate #:nst)
  :components
  ((:module "test"
    :components
    ((:module "iterate"
      :components ((:file "package")
                   (:file "run" :depends-on ("package"))))))))
