;;;; taxicab.lisp

(in-package #:grid-generators)

(defun taxicab-increment-at-one-distance (curs distance)
  (flet ((value (first rest)
           (values (list* first rest) t))
         (finish ()
           (values nil nil)))
    (cond
      ((and curs
            (plusp distance))
       (destructuring-bind (cur &rest rest) curs
         (cond
           ((null rest)
            (if (minusp cur)
                (value distance nil)
                (finish)))

           ((< cur distance)
            (let* ((rest-distance (- distance (abs cur)))
                   (left (taxicab-increment-at-one-distance rest
                                                            rest-distance)))
              (cond
                (left
                 (value cur left))
                (t
                 (let* ((cur (1+ cur))
                        (distance (- distance (abs cur)))
                        (left (mapcar (constantly 0) (rest rest))))
                   (value cur (list* (- distance) left)))))))
           (t
            (finish)))))
      (t
       (finish)))))

(defun taxicab-distance (coords)
  "Return the total taxicab distance from the origin represented by
the list of real numbers COORDS."
  (check-type coords list-of-reals)
  (reduce #'+ coords :key #'abs))

(defun taxicab-initial-value (distance dimensions)
  (when (plusp dimensions)
    (list* (- distance)
           (alexandria:iota (1- dimensions) :start 0 :step 0))))

(defun taxicab-increment (curs max-distance)
  (flet ((value (val)
           (values val t))
         (finish ()
           (values nil nil)))
    (let ((distance (taxicab-distance curs)))
      (multiple-value-bind (new got)
          (taxicab-increment-at-one-distance curs distance)
        (if got
            (value new)
            (let ((distance (taxicab-distance curs)))
              (if (or (null max-distance)
                      (< distance max-distance))
                  (value (taxicab-initial-value (1+ distance)
                                                (length curs)))
                  (finish))))))))

(defun make-taxicab-generator (dimensions &key (minimum-distance 0)
                                            (maximum-distance nil))
  "Given a positive integer number of DIMENSIONS, a non-negative
integer MINIMUM-DISTANCE, and a non-negative (or null)
MAXIMUM-DISTANCE, return a generator which, on successive calls,
returns the points in a taxicab-annulus with inner-radius
MINIMUM-DISTANCE and outer-radius MAXIMUM-DISTANCE.

MINIMUM-DISTANCE must be less than or equal to MAXIMUM-DISTANCE (if
MAXIMUM-DISTANCE is non-null).

If MAXIMUM-DISTANCE is null, the iteration continues on indefinitely.

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
all of the points at each distance."

  (check-type dimensions (integer 1 *))
  (check-type minimum-distance (integer 0 *))
  (check-type maximum-distance (or null (integer 0 *)))

  (when maximum-distance
    (assert (<= minimum-distance maximum-distance)))

  (let (last
        (got t))
    (lambda ()
      (cond
        ((and got (null last))
         (setf last (taxicab-initial-value minimum-distance dimensions)))

        (got
         (multiple-value-setq (last got)
           (taxicab-increment last maximum-distance))))
      (values last got))))
