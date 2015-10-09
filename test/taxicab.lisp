;;;; taxicab.lisp

(in-package #:grid-generators-tests)

(nst:def-test-group 1d-taxicab-generator-tests ()
  (nst:def-test 1d-taxicab (generates ((0) (-1) (1) (-2) (2)))
    (grid-generators:make-taxicab-generator 1
                                            :minimum-distance 0
                                            :maximum-distance 2))

  (nst:def-test 1d-taxicab-default-min (generates ((0) (-1) (1) (-2) (2)))
    (grid-generators:make-taxicab-generator 1
                                            :maximum-distance 2))

  (nst:def-test 1d-taxicab-annulus (generates ((-2) (2) (-3) (3)))
    (grid-generators:make-taxicab-generator 1
                                            :minimum-distance 2
                                            :maximum-distance 3))

  (nst:def-test 1d-taxicab-monotonic (monotonic
                                      :key #'grid-generators:taxicab-distance)
    (grid-generators:make-taxicab-generator 1
                                            :minimum-distance 1
                                            :maximum-distance 4))

  (nst:def-test 1d-taxicab-no-maximum (:equal 128)
    (loop
       :with g := (grid-generators:make-taxicab-generator 1)
       :for v := (funcall g)
       :repeat 257
       :maximizing (grid-generators:taxicab-distance v))))

(nst:def-test-group 2d-taxicab-generator-tests ()
  (nst:def-test 2d-taxicab (generates ((0 0) (-1 0) (1 0) (0 -1) (0 1)
                                       (-2 0) (2 0) (0 -2) (0 2)
                                       (-1 -1) (-1 1) (1 -1) (1 1)))
    (grid-generators:make-taxicab-generator 2
                                            :minimum-distance 0
                                            :maximum-distance 2))

  (nst:def-test 2d-taxicab-annulus (generates ((-3 0) (3 0)
                                               (-2 -1) (-2 1) (2 -1) (2 1)
                                               (-1 -2) (-1 2) (1 -2) (1 2)
                                               (0 -3) (0 3)))
    (grid-generators:make-taxicab-generator 2
                                            :minimum-distance 3
                                            :maximum-distance 3))

  (nst:def-test 2d-taxicab-scale-single-number (generates ((-6 0) (6 0)
                                                           (-4 -2) (-4 2)
                                                           (4 -2) (4 2)
                                                           (-2 -4) (-2 4)
                                                           (2 -4) (2 4)
                                                           (0 -6) (0 6)))
    (grid-generators:make-taxicab-generator 2
                                            :minimum-distance 3
                                            :maximum-distance 3
                                            :scale 2))

  (nst:def-test 2d-taxicab-scale-and-offset (generates ((-5 2) (7 2)
                                                        (-3 0) (-3 4)
                                                        (5 0) (5 4)
                                                        (-1 -2) (-1 6)
                                                        (3 -2) (3 6)
                                                        (1 -4) (1 8)))
    (grid-generators:make-taxicab-generator 2
                                            :minimum-distance 3
                                            :maximum-distance 3
                                            :scale 2
                                            :offset '(1 2)))

  (nst:def-test 2d-taxicab-scale-list (generates ((-6 0) (6 0)
                                                  (-4 -1/2) (-4 1/2)
                                                  (4 -1/2) (4 1/2)
                                                  (-2 -1) (-2 1)
                                                  (2 -1) (2 1)
                                                  (0 -3/2) (0 3/2)))
    (grid-generators:make-taxicab-generator 2
                                            :minimum-distance 3
                                            :maximum-distance 3
                                            :scale '(2 1/2)))

  (nst:def-test 2d-taxicab-monotonic (monotonic
                                      :key #'grid-generators:taxicab-distance)
    (grid-generators:make-taxicab-generator 3
                                            :minimum-distance 0
                                            :maximum-distance 3)))

(nst:def-test-group 4d-taxicab-generator-tests ()
  (nst:def-test 4d-taxicab (generates ((0 0 0 0)  ; distance 0

                                       (1 0 0 0) (-1 0 0 0) ; distance 1
                                       (0 1 0 0) (0 -1 0 0)
                                       (0 0 1 0) (0 0 -1 0)
                                       (0 0 0 1) (0 0 0 -1)

                                       (1 1 0 0) (-1 1 0 0) ; distance 2
                                       (1 -1 0 0) (-1 -1 0 0)
                                       (1 0 1 0) (-1 0 1 0)
                                       (1 0 -1 0) (-1 0 -1 0)
                                       (1 0 0 1) (-1 0 0 1)
                                       (1 0 0 -1) (-1 0 0 -1)

                                       (0 1 1 0) (0 -1 1 0)
                                       (0 1 -1 0) (0 -1 -1 0)
                                       (0 1 0 1) (0 -1 0 1)
                                       (0 1 0 -1) (0 -1 0 -1)

                                       (0 0 1 1) (0 0 -1 1)
                                       (0 0 1 -1) (0 0 -1 -1)

                                       (2 0 0 0) (-2 0 0 0)
                                       (0 2 0 0) (0 -2 0 0)
                                       (0 0 2 0) (0 0 -2 0)
                                       (0 0 0 2) (0 0 0 -2)))

    (grid-generators:make-taxicab-generator 4
                                            :minimum-distance 0
                                            :maximum-distance 2)))

(nst:def-test-group taxicab-bad-parameters-tests ()
  (nst:def-test dimensions-not-integer (:err)
    (grid-generators:make-taxicab-generator 1.5))

  (nst:def-test dimensions-not-positive (:err)
    (grid-generators:make-taxicab-generator 0))

  (nst:def-test minimum-not-integer (:err)
    (grid-generators:make-taxicab-generator 1
                                            :minimum-distance 1.5))

  (nst:def-test minimum-not-non-negative (:err)
    (grid-generators:make-taxicab-generator 1
                                            :minimum-distance -1))

  (nst:def-test maximum-not-integer (:err)
    (grid-generators:make-taxicab-generator 1
                                            :maximum-distance 1.5))

  (nst:def-test maximum-not-non-negative (:err)
    (grid-generators:make-taxicab-generator 1
                                            :maximum-distance -1))

  (nst:def-test maximum-less-than-minimum (:err)
    (grid-generators:make-taxicab-generator 1
                                            :minimum-distance 2
                                            :maximum-distance 1))

  (nst:def-test scale-not-a-number (:err)
    (grid-generators:make-taxicab-generator 1
                                            :minimum-distance 2
                                            :scale :a))

  (nst:def-test scale-not-a-list-of-number (:err)
    (grid-generators:make-taxicab-generator 1
                                            :minimum-distance 2
                                            :scale '(:a)))

  (nst:def-test scale-not-a-correct-length (:err)
    (grid-generators:make-taxicab-generator 1
                                            :minimum-distance 2
                                            :scale '(1 2)))

  (nst:def-test offset-not-a-list (:err)
    (grid-generators:make-taxicab-generator 1
                                            :minimum-distance 2
                                            :offset 1))

  (nst:def-test offset-not-a-list-of-numbers (:err)
    (grid-generators:make-taxicab-generator 1
                                            :minimum-distance 2
                                            :offset '(:a)))

  (nst:def-test offset-not-right-length (:err)
    (grid-generators:make-taxicab-generator 1
                                            :minimum-distance 2
                                            :offset '(1 2 4))))
