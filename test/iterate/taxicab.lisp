;;;; taxicab.lisp

(in-package #:grid-iterate-tests)

(nst:def-test-group 1d-taxicab-iterate-tests ()
  (nst:def-test 1d-taxicab (generates ((0) (-1) (1) (-2) (2)))
    (collect (v)
      by-taxicab-steps-in-dimensions 1 minimum-steps 0 maximum-steps 2))

  (nst:def-test 1d-taxicab-default-min (generates ((0) (-1) (1) (-2) (2)))
    (collect (v)
      by-taxicab-steps-in-dimensions 1 maximum-steps 2))

  (nst:def-test 1d-taxicab-annulus (generates ((-2) (2) (-3) (3)))
    (collect (v)
      by-taxicab-steps-in-dimensions 1 minimum-steps 2 maximum-steps 3))

  (nst:def-test 1d-taxicab-monotonic (monotonic
                                      :key #'grid-generators:taxicab-distance)
    (collect (v)
      by-taxicab-steps-in-dimensions 1 minimum-steps 1 maximum-steps 4))

  (nst:def-test 1d-taxicab-no-maximum (:equal 128)
    (iterate:iterate
      (iterate:for v by-taxicab-steps-in-dimensions 1)
      (iterate:for i from 1 to 257)
      (iterate:maximize (grid-generators:taxicab-distance v)))))

(nst:def-test-group 2d-taxicab-iterate-tests ()
  (nst:def-test 2d-taxicab (generates ((0 0) (-1 0) (1 0) (0 -1) (0 1)
                                       (-2 0) (2 0) (0 -2) (0 2)
                                       (-1 -1) (-1 1) (1 -1) (1 1)))
    (collect (v)
      by-taxicab-steps-in-dimensions 2 minimum-steps 0 maximum-steps 2))

  (nst:def-test 2d-taxicab-annulus (generates ((-3 0) (3 0)
                                               (-2 -1) (-2 1) (2 -1) (2 1)
                                               (-1 -2) (-1 2) (1 -2) (1 2)
                                               (0 -3) (0 3)))
    (collect (v)
      by-taxicab-steps-in-dimensions 2 minimum-steps 3 maximum-steps 3))

  (nst:def-test 2d-taxicab-scale-single-number (generates ((-6 0) (6 0)
                                                           (-4 -2) (-4 2)
                                                           (4 -2) (4 2)
                                                           (-2 -4) (-2 4)
                                                           (2 -4) (2 4)
                                                           (0 -6) (0 6)))
    (collect (v)
      by-taxicab-steps-in-dimensions 2 minimum-steps 3 maximum-steps 3
                                       with-scale 2))

  (nst:def-test 2d-taxicab-scale-and-offset (generates ((-5 2) (7 2)
                                                        (-3 0) (-3 4)
                                                        (5 0) (5 4)
                                                        (-1 -2) (-1 6)
                                                        (3 -2) (3 6)
                                                        (1 -4) (1 8)))
    (collect (v)
      by-taxicab-steps-in-dimensions 2 minimum-steps 3 maximum-steps 3
                                       with-scale 2 with-offset '(1 2)))

  (nst:def-test 2d-taxicab-scale-list (generates ((-6 0) (6 0)
                                                  (-4 -1/2) (-4 1/2)
                                                  (4 -1/2) (4 1/2)
                                                  (-2 -1) (-2 1)
                                                  (2 -1) (2 1)
                                                  (0 -3/2) (0 3/2)))
    (collect (v)
      by-taxicab-steps-in-dimensions 2 minimum-steps 3 maximum-steps 3
                                       with-scale '(2 1/2)))

  (nst:def-test 2d-taxicab-monotonic (monotonic
                                      :key #'grid-generators:taxicab-distance)
    (collect (v)
      by-taxicab-steps-in-dimensions 2 minimum-steps 0 maximum-steps 3)))

(nst:def-test-group 4d-taxicab-iterate-tests ()
  (nst:def-test 4d-taxicab (generates ((0 0 0 0)  ; steps 0

                                       (1 0 0 0) (-1 0 0 0) ; steps 1
                                       (0 1 0 0) (0 -1 0 0)
                                       (0 0 1 0) (0 0 -1 0)
                                       (0 0 0 1) (0 0 0 -1)

                                       (1 1 0 0) (-1 1 0 0) ; steps 2
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

    (collect (v)
      by-taxicab-steps-in-dimensions 4 minimum-steps 0 maximum-steps 2)))

(nst:def-test-group taxicab-iterate-bad-parameters-tests ()
  (nst:def-test dimensions-not-integer (:err)
    (collect (v)
      by-taxicab-steps-in-dimensions 1.5))

  (nst:def-test dimensions-not-positive (:err)
    (collect (v)
      by-taxicab-steps-in-dimensions 0))

  (nst:def-test minimum-not-integer (:err)
    (collect (v)
      by-taxicab-steps-in-dimensions 1 minimum-steps 1.5))

  (nst:def-test minimum-not-non-negative (:err)
    (collect (v)
      by-taxicab-steps-in-dimensions 1 minimum-steps -1))

  (nst:def-test maximum-not-integer (:err)
    (collect (v)
      by-taxicab-steps-in-dimensions 1 maximum-steps 1.5))

  (nst:def-test maximum-not-non-negative (:err)
    (collect (v)
      by-taxicab-steps-in-dimensions 1 maximum-steps -1))

  (nst:def-test maximum-less-than-minimum (:err)
    (collect (v)
      by-taxicab-steps-in-dimensions 1 minimum-steps 2 maximum-steps 1))

  (nst:def-test scale-not-a-number (:err)
    (collect (v)
      by-taxicab-steps-in-dimensions 1 minimum-steps 2 with-scale :a))

  (nst:def-test scale-not-a-list-of-number (:err)
    (collect (v)
      by-taxicab-steps-in-dimensions 1 minimum-steps 2 with-scale '(:a)))

  (nst:def-test scale-not-a-correct-length (:err)
    (collect (v)
      by-taxicab-steps-in-dimensions 1 minimum-steps 2 with-scale '(1 2)))

  (nst:def-test offset-not-a-list (:err)
    (collect (v)
      by-taxicab-steps-in-dimensions 1 minimum-steps 2 offset 1))

  (nst:def-test offset-not-a-list-of-numbers (:err)
    (collect (v)
      by-taxicab-steps-in-dimensions 1 minimum-steps 2 offset '(:a)))

  (nst:def-test offset-not-right-length (:err)
    (collect (v)
      by-taxicab-steps-in-dimensions 1 minimum-steps 2 offset '(1 2 4))))
