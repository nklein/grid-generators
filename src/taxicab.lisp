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
  (check-type coords (list-of+ real))
  (reduce #'+ coords :key #'abs))

(defun taxicab-initial-value (distance dimensions)
  (when (plusp dimensions)
    (list* (- distance)
           (loop :repeat (1- dimensions) :collecting 0))))

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

(defun make-taxicab-generator (dimensions &key (minimum-steps 0)
                                            (maximum-steps nil)
                                            (scale nil)
                                            (offset nil))
  "Given a positive integer number of DIMENSIONS, a non-negative
integer MINIMUM-STEPS, and a non-negative (or null)
MAXIMUM-STEPS, return a generator which, on successive calls,
returns the points in a taxicab-annulus with inner-radius
MINIMUM-STEPS and outer-radius MAXIMUM-STEPS.

If SCALE is specified, it must be either a list of length DIMENSIONS
of NUMBERs or a single NUMBER.  The taxicab coordinates will be scaled
respectively by these amounts (or all by this amount if it is a single
number) before they are returned.

If OFFSET is specified, it must be a list of length DIMENSIONS of
NUMBERs.  This offset will be added to the taxicab coordinates (after
they are scaled and) before they are returned.

MINIMUM-STEPS must be less than or equal to MAXIMUM-STEPS (if
MAXIMUM-STEPS is non-null).

If MAXIMUM-STEPS is null, the iteration continues on indefinitely.

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
of the points at each number of steps."

  (check-type dimensions (integer 1 *))
  (check-type minimum-steps (integer 0 *))
  (check-type maximum-steps (or null (integer 0 *)))
  (check-type scale (or number (list-of number)))
  (check-type offset (list-of number))

  (when maximum-steps
    (assert (<= minimum-steps maximum-steps)))

  (when (consp scale)
    (assert (= (length scale) dimensions)))

  (when (consp offset)
    (assert (= (length offset) dimensions)))

  (let (last
        (got t)
        (scale (if (numberp scale)
                   (loop :repeat dimensions :collecting scale)
                   scale)))
    (lambda ()
      (cond
        ((and got (null last))
         (setf last (taxicab-initial-value minimum-steps dimensions)))

        (got
         (multiple-value-setq (last got)
           (taxicab-increment last maximum-steps))))

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
