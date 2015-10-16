GRID-GENERATORS Package
=======================

This package provides functions useful for generating the points in a
grid.  There are three generator constructors.  Each returns a function.
Upon successive calls to the returned function, it returns two values:
the coordinates of the next point in the grid (or `NIL` if the grid
has been exhausted) and `T` if a grid point was returned as the first
value (or `NIL` if the grid has been exhausted).

Generating Points In A Cube
---------------------------

To generate points in a cube, use the generator:

    (make-grid-generator TO &KEY FROM BY)

Given a list of `REAL` numbers `TO` and a list of `REAL` numbers
`FROM` and a list of real numbers `BY` (all of length `N`), return a
function that on successive calls will return points on an
`N`-dimensional grid starting at `FROM` and incrementing by `BY`.

`FROM` defaults to all zeros if not specified.

`BY` defaults to all ones if not specified.

All of the `BY` numbers must be positive.

All of the `FROM` numbers must be less than or equal to their
corresponding `TO` numbers.

For example:

    (loop :with generator := (make-grid-generator '(2 2) :by '(1 2))
        :for v := (funcall generator)
        :while v
        :collecting v)

    => ((0 0) (1 0) (2 0) (0 2) (1 2) (2 2))

Note: This function makes no guarantees about the order in which the
grid points will be covered.  It only guarantees that they all will be
covered.

Generating Points By Taxicab Distance
-------------------------------------

The taxicab distance from the origin to some point `P` is just the sum
of the absolute values of its coordinates.  This differs from the
Euclidean distance.  Given a list of coordinates `P` for the point,
you can calculate the taxicab distance from the origin to that point
with:

    (taxicab-distance P)

To generate all of the grid points in `DIMENSIONS`-dimensional space with
integer number of steps in the cardinal directions, use the generator:

    (make-taxicab-generator DIMENSIONS
                            &KEY (MINIMUM-STEPS 0)
                                 MAXIMUM-STEPS
                                 SCALE
                                 OFFSET)

Given a positive integer number of `DIMENSIONS`, a non-negative
integer `MINIMUM-STEPS`, and a non-negative (or null)
`MAXIMUM-STEPS`, return a generator which, on successive calls,
returns the points in a taxicab-annulus with inner-radius
`MINIMUM-STEPS` and outer-radius `MAXIMUM-STEPS`.

`MINIMUM-STEPS` must be less than or equal to `MAXIMUM-STEPS` (if
`MAXIMUM-STEPS` is non-null).

If `SCALE` is specified, it must be either a list of length `DIMENSIONS`
of `NUMBER`s or a single `NUMBER`.  The taxicab coordinates will be scaled
respectively by these amounts (or all by this amount if it is a single
number) before they are returned.

If `OFFSET` is specified, it must be a list of length `DIMENSIONS` of
`NUMBER`s.  This offset will be added to the taxicab coordinates (after
they are scaled and) before they are returned.

If `MAXIMUM-STEPS` is null, the iteration continues on indefinitely.

For example:

    (loop :with generator = (make-taxicab-generator 2
                                                    :maximum-steps 2
                                                    :scale 2
                                                    :offset '(1 0))
        :for v := (funcall generator)
        :while v
        :collecting v)

    => ((1 0)
        (-1 0) (1 -2) (1 2) (3 0)
        (-3 0) (-1 -2) (-1 2) (1 -4) (1 4) (3 -2) (3 2) (5 0))

Note: When the scaling is not specified or is constant, this function
guarantees that it will generate all of the points at the minimum
steps before moving on to the next greater number of steps, etc.  It
does not, however, guarantee the order in which it will generate the
points at a given number of steps away, just that it will generate all
of the points at each number of steps.

Generating Points In An Arbitrary Lattice
-----------------------------------------

Given a list of `BASIS-VECTORS` (each a list of `NUMBER`s with all
lists the same length), a non-negative integer `MINIMUM-STEPS`, and a
non-negative (or null) `MAXIMUM-STEPS`, return a generator which, on
successive calls, returns points on the scaled lattice centered at the
offset with the given basis vectors starting with those points which
can be reached with the `MINIMUM-STEPS` and proceeding through more and
more steps until those points at `MAXIMUM-STEPS` have all been
enumerated.

If `SCALE` is specified, it must be either a list of the same length as
the basis vectors of `NUMBER`s or a single `NUMBER`.  The point's
coordinates will be scaled by this amount before they are returned.

If `OFFSET` is specified, it must be a list of same length as the basis
vectors of `NUMBER`s.  This offset will be added to the
coordinates (after they are scaled and) before they are returned.

`MINIMUM-STEPS` must be less than or equal to `MAXIMUM-STEPS` (if
`MAXIMUM-STEPS` is non-null).

If `MAXIMUM-STEPS` is null, the iteration continues on indefinitely.

For example:

    (loop :with generator = (make-lattice-generator '((#C(1 0)) (#C(0 1)))
                                                    :maximum-steps 1
                                                    :scale 2
                                                    :offset '(#C(1/2 1/2)))
        :for v := (funcall generator)
        :while v
        :collecting (first v))

    => (#C(1/2 1/2) #C(-3/2 1/2) #C(1/2 -3/2) #C(1/2 5/2) #C(5/2 1/2))


GRID-ITERATE Package
====================

This package provides `ITERATE` drivers for the generators in the
`GRID-GENERATORS` package.

Iterating Points In A Cube
--------------------------

The iterate clause for generating the points on the grid is:

    (ITERATE:FOR var ON-GRID-TO to &optional FROM from BY by)

These parameters relate directly to their namesakes in the
`GRID-GENERATORS` package.  So, for example, you could:

    (iterate:iterate (iterate:for v on-grid-to '(2 2) by '(1 2))
                     (iterate:collecting v))

    => ((0 0) (1 0) (2 0) (0 2) (1 2) (2 2))


Iterating Points By Taxicab Distance
------------------------------------

The iterate clause for generating points by taxicab distance is:

    (ITERATE:FOR var BY-TAXICAB-STEPS-IN-DIMENSIONS dimensions
             &optional MINIMUM-STEPS min-steps
                       MAXIMUM-STEPS max-steps
                       WITH-SCALE scale
                       WITH-OFFSET offset)

These parameters relate directly to their namesakes in the
`GRID-GENERATORS` package.  So, for example, you could:

    (iterate:iterate (iterate:for v by-taxicab-steps-in-dimensions 2
                               maximum-steps 2
                               with-scale 2
                               with-offset '(1 0))
                     (iterate:collect v))

    => ((1 0)
        (-1 0) (1 -2) (1 2) (3 0)
        (-3 0) (-1 -2) (-1 2) (1 -4) (1 4) (3 -2) (3 2) (5 0))

Iterating Points In An Arbitrary Lattice
----------------------------------------

The iterate clause for generating points in an arbitrary lattice is:

    (ITERATE:FOR var BY-LATTICE-STEPS basis-vectors
             &optional MINIMUM-STEPS min-steps
                       MAXIMUM-STEPS max-steps
                       WITH-SCALE scale
                       WITH-OFFSET offset)

These parameters relate directly to their namesakes in the
`GRID-GENERATORS` package.  So, for example, you could:

    (iterate:iterate (iterate:for v by-lattice-steps '((1 -1) (1 1))
                               maximum-steps 2
                               with-scale 2
                               with-offset '(1 0))
                     (iterate:collect v))

    => ((1 0)
        (-1 2) (-1 -2) (3 2) (3 -2)
        (-3 4) (-3 0) (1 4) (-3 -4) (5 4) (1 -4) (5 0) (5 -4))
