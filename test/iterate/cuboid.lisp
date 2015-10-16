;;;; cuboid.lisp

(in-package #:grid-iterate-tests)

(nst:def-test-group 1d-grid-iterate-tests ()
  (nst:def-test 1d-grid (generates ((1) (4) (7)))
    (collect (v) on-grid-to '(7) from '(1) by '(3)))

  (nst:def-test 1d-grid-default-by (generates ((1) (2) (3)))
    (collect (v) on-grid-to '(3) from '(1)))

  (nst:def-test 1d-grid-default-from (generates ((0) (2) (4)))
    (collect (v) on-grid-to '(4) by '(2)))

  (nst:def-test 1d-grid-off-center (generates ((0) (2)))
    (collect (v) on-grid-to '(3) by '(2))))

(nst:def-test-group 2d-grid-iterate-tests ()
  (nst:def-test 2d-grid (generates ((0 0) (1 0) (2 0)
                                    (0 2) (1 2) (2 2)))
    (collect (v) on-grid-to '(2 2) by '(1 2)))

  (nst:def-test 2d-grid-symmetric (generates ((0 -2) (1 -2) (2 -2)
                                              (0  0) (1  0) (2  0)
                                              (0  2) (1  2) (2  2)))
    (collect (v) on-grid-to '(2 2) from '(0 -2) by '(1 2))))

(nst:def-test-group grid-iterate-bad-parameters-tests ()
  (nst:def-test to-not-list (:err)
    (collect (v) on-grid-to 3))

  (nst:def-test to-not-numbers (:err)
    (collect (v) on-grid-to '(:a :b)))

  (nst:def-test from-not-numbers (:err)
    (collect (v) on-grid-to '(2 2) from '(:a :b)))

  (nst:def-test by-not-numbers (:err)
    (collect (v) on-grid-to '(2 2) by '(:a :b)))

  (nst:def-test by-not-positive (:err)
    (collect (v) on-grid-to '(2 2) by '(0 0)))

  (nst:def-test from-not-less-than-or-equal-to-to (:err)
    (collect (v) on-grid-to '(2 2) from '(2 3)))

  (nst:def-test from-not-correct-length (:err)
    (collect (v) on-grid-to '(2 2) from '(0)))

  (nst:def-test by-not-correct-length (:err)
    (collect (v) on-grid-to '(2 2) by '(1))))
