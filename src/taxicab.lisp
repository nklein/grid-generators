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
                                            (maximum-distance nil)
                                            (scale nil)
                                            (offset nil))
  "Given a positive integer number of DIMENSIONS, a non-negative
integer MINIMUM-DISTANCE, and a non-negative (or null)
MAXIMUM-DISTANCE, return a generator which, on successive calls,
returns the points in a taxicab-annulus with inner-radius
MINIMUM-DISTANCE and outer-radius MAXIMUM-DISTANCE.

If SCALE is specified, it must be either a list of length DIMENSIONS
of NUMBERs or a single NUMBER.  The taxicab coordinates will be scaled
respectively by these amounts (or all by this amount if it is a single
number) before they are returned.

If OFFSET is specified, it must be a list of length DIMENSIONS of
NUMBERs.  This offset will be added to the taxicab coordinates (after
they are scaled and) before they are returned.

MINIMUM-DISTANCE must be less than or equal to MAXIMUM-DISTANCE (if
MAXIMUM-DISTANCE is non-null).

If MAXIMUM-DISTANCE is null, the iteration continues on indefinitely.

For example:

    (loop :with generator = (make-taxicab-generator 2
                                                    :maximum-distance 2
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
distance before moving on to the next greater distance, etc.  It does
not, however, guarantee the order in which it will generate the points
at a given distance, just that it will generate all of the points at
each distance."

  (check-type dimensions (integer 1 *))
  (check-type minimum-distance (integer 0 *))
  (check-type maximum-distance (or null (integer 0 *)))
  (check-type scale (or null number list))
  (check-type offset (or null list))

  (when maximum-distance
    (assert (<= minimum-distance maximum-distance)))

  (when (consp scale)
    (assert (every #'numberp scale))
    (assert (= (length scale) dimensions)))

  (when offset
    (assert (every #'numberp offset))
    (assert (= (length offset) dimensions)))

  (let (last
        (got t)
        (scale (if (numberp scale)
                   (loop :repeat dimensions :collecting scale)
                   scale)))
    (lambda ()
      (cond
        ((and got (null last))
         (setf last (taxicab-initial-value minimum-distance dimensions)))

        (got
         (multiple-value-setq (last got)
           (taxicab-increment last maximum-distance))))

      (flet ((transform (v)
               (let* ((v (if scale
                             (mapcar #'* v scale)
                             v))
                      (v (if offset
                             (mapcar #'+ v offset)
                             v)))
                 v)))
        (values (when last
                  (transform last))
                got)))))
