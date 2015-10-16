;;;; lattice.lisp

(in-package #:grid-generators)

(defun step-in-lattice (steps basis-vectors)
  (flet ((vec+ (a b)
           (mapcar #'+ a b))
         (vec* (s v)
           (mapcar (lambda (c) (* s c)) v)))
    (reduce #'vec+ (mapcar #'vec* steps basis-vectors))))

(ensure-sequence-type number)
(ensure-sequence-type+ number)
(ensure-sequence-type+ (list-of+ number))

(defun make-lattice-generator (basis-vectors
                               &key (minimum-steps 0)
                                 (maximum-steps nil)
                                 (scale nil)
                                 (offset nil))
  "Given a list of BASIS-VECTORS (each a list of NUMBERs with all
lists the same length), a non-negative integer MINIMUM-STEPS, and a
non-negative (or null) MAXIMUM-STEPS, return a generator which, on
successive calls, returns points on the scaled lattice centered at the
offset with the given basis vectors starting with those points which
can be reached with the MINIMUM-STEPS and proceeding through more and
more steps until those points at MAXIMUM-STEPS have all been
enumerated.

If SCALE is specified, it must be either a list of the same length as
the basis vectors of NUMBERs or a single NUMBER.  The point's
coordinates will be scaled by this amount before they are returned.

If OFFSET is specified, it must be a list of same length as the basis
vectors of NUMBERs.  This offset will be added to the
coordinates (after they are scaled and) before they are returned.

MINIMUM-STEPS must be less than or equal to MAXIMUM-STEPS (if
MAXIMUM-STEPS is non-null).

If MAXIMUM-STEPS is null, the iteration continues on indefinitely.

For example:

    (loop :with generator = (make-lattice-generator '((#C(1 0)) (#C(0 1)))
                                                    :maximum-steps 1
                                                    :scale 2
                                                    :offset '(#C(1/2 1/2)))
        :for v := (funcall generator)
        :while v
        :collecting (first v))

    => (#C(1/2 1/2) #C(-3/2 1/2) #C(1/2 -3/2) #C(1/2 5/2) #C(5/2 1/2))
"

  (check-type basis-vectors (list-of+ (list-of+ number)))
  (check-type minimum-steps (integer 0 *))
  (check-type maximum-steps (or null (integer 0 *)))
  (check-type scale (or number (list-of number)))
  (check-type offset (list-of number))

  (let ((dimensions (length (first basis-vectors))))

    (every (lambda (v)
             (assert (= (length v) dimensions)))
           (rest basis-vectors))

    (when maximum-steps
      (assert (<= minimum-steps maximum-steps)))

    (when (consp scale)
      (assert (= (length scale) dimensions)))

    (when (consp offset)
      (assert (= (length offset) dimensions)))

    (let ((scale (if (numberp scale)
                     (loop :repeat dimensions :collecting scale)
                     scale))
          (taxicab (make-taxicab-generator (length basis-vectors)
                                           :minimum-steps minimum-steps
                                           :maximum-steps maximum-steps)))

      (flet ((transform (v)
               (let* ((v (step-in-lattice v basis-vectors))
                      (v (if scale
                             (mapcar #'* v scale)
                             v))
                      (v (if offset
                             (mapcar #'+ v offset)
                             v)))
                 v)))
        (lambda ()
          (multiple-value-bind (v got) (funcall taxicab)
            (values (when got
                      (transform v))
                    got)))))))
