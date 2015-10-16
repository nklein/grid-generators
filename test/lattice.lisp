;;;; lattice.lisp

(in-package #:grid-generators-tests)

(nst:def-test-group 1d-lattice-generator-tests ()
  (nst:def-test 1d-lattice (generates ((0 0) (-1 -2) (1 2) (-2 -4) (2 4)))
    (grid-generators:make-lattice-generator '((1 2))
                                            :minimum-steps 0
                                            :maximum-steps 2))

  (nst:def-test 1d-lattice-default-min (generates ((0 0)
                                                   (-1 -2) (1 2)
                                                   (-2 -4) (2 4)))
    (grid-generators:make-lattice-generator '((1 2))
                                            :maximum-steps 2))

  (nst:def-test 1d-lattice-annulus (generates ((-2 -4) (2 4) (-3 -6) (3 6)))
    (grid-generators:make-lattice-generator '((1 2))
                                            :minimum-steps 2
                                            :maximum-steps 3))

  (nst:def-test 1d-lattice-monotonic (monotonic
                                      :key #'grid-generators:taxicab-distance)
    (grid-generators:make-lattice-generator '((1 1))
                                            :minimum-steps 1
                                            :maximum-steps 4))

  (nst:def-test 1d-taxicab-no-maximum (:equal 256)
    (loop
       :with g := (grid-generators:make-lattice-generator '((1 1)))
       :for v := (funcall g)
       :repeat 257
       :maximizing (grid-generators:taxicab-distance v))))

(nst:def-test-group 2d-lattice-generator-tests ()
  (nst:def-test 2d-lattice (generates ((0 0 0) (-1 0 0) (1 0 0) (0 0 -1) (0 0 1)
                                       (-2 0 0) (2 0 0) (0 0 -2) (0 0 2)
                                       (-1 0 -1) (-1 0 1) (1 0 -1) (1 0 1)))
    (grid-generators:make-lattice-generator '((1 0 0) (0 0 1))
                                            :minimum-steps 0
                                            :maximum-steps 2))

  (nst:def-test 2d-lattice-annulus (generates ((-3 0 0) (3 0 0)
                                               (-2 -1 -2) (-2 1 2)
                                               (2 -1 -2) (2 1 2)
                                               (-1 -2 -4) (-1 2 4)
                                               (1 -2 -4) (1 2 4)
                                               (0 -3 -6) (0 3 6)))
    (grid-generators:make-lattice-generator '((0 1 2) (1 0 0))
                                            :minimum-steps 3
                                            :maximum-steps 3))

  (nst:def-test 2d-lattice-scale-single-number (generates ((-6 0) (6 0)
                                                           (-4 -2) (-4 2)
                                                           (4 -2) (4 2)
                                                           (-2 -4) (-2 4)
                                                           (2 -4) (2 4)
                                                           (0 -6) (0 6)))
    (grid-generators:make-lattice-generator '((0 1) (-1 0))
                                            :minimum-steps 3
                                            :maximum-steps 3
                                            :scale 2))

  (nst:def-test 2d-lattice-scale-and-offset (generates ((-5 -4) (-5 0)
                                                        (-1 -4) (-5 4)
                                                        (3 -4) (-5 8)
                                                        (7 -4) (-1 8)
                                                        (7 0) (3 8)
                                                        (7 4) (7 8)))
    (grid-generators:make-lattice-generator '((1 1) (1 -1))
                                            :minimum-steps 3
                                            :maximum-steps 3
                                            :scale 2
                                            :offset '(1 2)))

  (nst:def-test 2d-lattice-scale-list (generates ((-6 0) (6 0)
                                                  (-4 -1) (-4 1)
                                                  (4 -1) (4 1)
                                                  (-2 -2) (-2 2)
                                                  (2 -2) (2 2)
                                                  (0 -3) (0 3)))
    (grid-generators:make-lattice-generator '((1 0) (0 2))
                                            :minimum-steps 3
                                            :maximum-steps 3
                                            :scale '(2 1/2)))

  (nst:def-test 2d-lattice-monotonic (monotonic
                                      :key #'grid-generators:taxicab-distance)
    (grid-generators:make-lattice-generator '((1 1 0 0) (0 0 1 1))
                                            :minimum-steps 0
                                            :maximum-steps 3)))

(nst:def-test-group coin-tests ()
  (nst:def-test two-coin-combinations (generates (;; no coins
                                                  (0)
                                                  ;; one coin (+)
                                                  (1) (5) (10) (25)
                                                  ;; one coin (-)
                                                  (-1) (-5) (-10) (-25)
                                                  ;; two coins (++)
                                                  (2) (10) (20) (50)
                                                  (6) (11) (26)
                                                  (15) (30)
                                                  (35)
                                                  ;; two coins (--)
                                                  (-2) (-10) (-20) (-50)
                                                  (-6) (-11) (-26)
                                                  (-15) (-30)
                                                  (-35)
                                                  ;; two coins (+-)
                                                  (-4) (-9) (-24)
                                                  (-5) (-20)
                                                  (-15)
                                                  ;; two coins (-+)
                                                  (4) (9) (24)
                                                  (5) (20)
                                                  (15)))
    (grid-generators:make-lattice-generator '((1) (5) (10) (25))
                                            :maximum-steps 2))

  (nst:def-test balanced-ternary (generates ((0)
                                            (-1) (1) (-3) (3)
                                            (-2) (2) (-4) (4) (-2) (2)
                                            (-6) (6)))
    (grid-generators:make-lattice-generator '((1) (3))
                                            :maximum-steps 2)))

(nst:def-test-group 4d-lattice-generator-tests ()
  (nst:def-test 4d-lattice (generates ((0 0 0 0)  ; steps 0

                                       (1 0 0 0) (-1 0 0 0) ; steps 1
                                       (0 2 0 0) (0 -2 0 0)
                                       (0 0 3 0) (0 0 -3 0)
                                       (0 0 0 4) (0 0 0 -4)

                                       (1 2 0 0) (-1 2 0 0) ; steps 2
                                       (1 -2 0 0) (-1 -2 0 0)
                                       (1 0 3 0) (-1 0 3 0)
                                       (1 0 -3 0) (-1 0 -3 0)
                                       (1 0 0 4) (-1 0 0 4)
                                       (1 0 0 -4) (-1 0 0 -4)

                                       (0 2 3 0) (0 -2 3 0)
                                       (0 2 -3 0) (0 -2 -3 0)
                                       (0 2 0 4) (0 -2 0 4)
                                       (0 2 0 -4) (0 -2 0 -4)

                                       (0 0 3 4) (0 0 -3 4)
                                       (0 0 3 -4) (0 0 -3 -4)

                                       (2 0 0 0) (-2 0 0 0)
                                       (0 4 0 0) (0 -4 0 0)
                                       (0 0 6 0) (0 0 -6 0)
                                       (0 0 0 8) (0 0 0 -8)))

    (grid-generators:make-lattice-generator '((1 0 0 0)
                                              (0 2 0 0)
                                              (0 0 3 0)
                                              (0 0 0 4))
                                            :minimum-steps 0
                                            :maximum-steps 2)))

(nst:def-test-group lattice-bad-parameters-tests ()
  (nst:def-test basis-null (:err)
    (grid-generators:make-lattice-generator nil))

  (nst:def-test dimensions-not-positive (:err)
    (grid-generators:make-lattice-generator '(nil)))

  (nst:def-test minimum-not-integer (:err)
    (grid-generators:make-lattice-generator '((1))
                                            :minimum-steps 1.5))

  (nst:def-test minimum-not-non-negative (:err)
    (grid-generators:make-lattice-generator '((1))
                                            :minimum-steps -1))

  (nst:def-test maximum-not-integer (:err)
    (grid-generators:make-lattice-generator '((1))
                                            :maximum-steps 1.5))

  (nst:def-test maximum-not-non-negative (:err)
    (grid-generators:make-lattice-generator '((1))
                                            :maximum-steps -1))

  (nst:def-test maximum-less-than-minimum (:err)
    (grid-generators:make-lattice-generator '((1))
                                            :minimum-steps 2
                                            :maximum-steps 1))

  (nst:def-test scale-not-a-number (:err)
    (grid-generators:make-lattice-generator '((1))
                                            :minimum-steps 2
                                            :scale :a))

  (nst:def-test scale-not-a-list-of-number (:err)
    (grid-generators:make-lattice-generator '((1))
                                            :minimum-steps 2
                                            :scale '(:a)))

  (nst:def-test scale-not-right-length (:err)
    (grid-generators:make-lattice-generator '((1) (2))
                                            :minimum-steps 2
                                            :scale '(1 2)))

  (nst:def-test offset-not-a-list (:err)
    (grid-generators:make-lattice-generator '((1))
                                            :minimum-steps 2
                                            :offset 1))

  (nst:def-test offset-not-a-list-of-numbers (:err)
    (grid-generators:make-lattice-generator '((1))
                                            :minimum-steps 2
                                            :offset '(:a)))

  (nst:def-test offset-not-right-length (:err)
    (grid-generators:make-lattice-generator '((1) (2) (3))
                                            :minimum-steps 2
                                            :offset '(1 2 4))))
