GRID-GENERATORS Package
=======================

This package provides functions useful for generating the points in a
grid.  There are two generator constructors.  Each returns a function.
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
integer taxicab distance from the origin, use the generator:

    (make-taxicab-generator DIMENSIONS
                            &KEY (MINIMUM-DISTANCE 0)
                                 MAXIMUM-DISTANCE)

Given a positive integer number of `DIMENSIONS`, a non-negative
integer `MINIMUM-DISTANCE`, and a non-negative (or null)
`MAXIMUM-DISTANCE`, return a generator which, on successive calls,
returns the points in a taxicab-annulus with inner-radius
`MINIMUM-DISTANCE` and outer-radius `MAXIMUM-DISTANCE`.

`MINIMUM-DISTANCE` must be less than or equal to `MAXIMUM-DISTANCE` (if
`MAXIMUM-DISTANCE` is non-null).

If `MAXIMUM-DISTANCE` is null, the iteration continues on indefinitely.

For example:

    (loop :with generator = (make-taxicab-generator 2 :maximum-distance 2)
        :for v := (funcall generator)
        :while v
        :collecting v)

    => ((0 0)
        (-1 0) (0 -1) (0 1) (1 0)
        (-2 0) (-1 -1) (-1 1) (0 -2) (0 2) (1 -1) (1 1) (2 0))

Note: This function guarantees that it will generate all of the points
at the minimum distance before moving on to the next greater distance,
etc.  It does not, however, guarantee the order in which it will
generate the points at a given distance, just that it will generate
all of the points at each distance.