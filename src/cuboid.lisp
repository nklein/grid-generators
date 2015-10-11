;;;; cuboid.lisp

(in-package #:grid-generators)

(defun grid-increment (curs tos froms bys)
  (flet ((value (first rest)
           (values (list* first rest) t))
         (finish ()
           (values nil nil)))

    (cond
      ((and curs tos froms bys)
       (let-plus:let+ (((cur &rest curs) curs)
                       ((to &rest tos) tos)
                       ((from &rest froms) froms)
                       ((by &rest bys) bys))

         (let ((next (+ cur by)))
           (if (<= next to)
               (value next curs)
               (alexandria:if-let ((rest (grid-increment curs tos froms bys)))
                 (value from rest)
                 (finish))))))
      (t
       (finish)))))

(ensure-sequence-type+ real)

(defun make-grid-generator (to &key from by)
  "Given a list of REAL numbers TO and a list of REAL numbers FROM and
a list of real numbers BY (all of length N), return a function that on
successive calls will return points on an N-dimensional grid starting
at FROM and incrementing by BY.

FROM defaults to all zeros if not specified.

BY defaults to all ones if not specified.

All of the BY numbers must be positive.

All of the FROM numbers must be less than or equal to their
corresponding TO numbers.

For example:

    (loop :with generator = (make-grid-generator '(2 2) :by '(1 2))
        :for v := (funcall generator)
        :while v
        :collecting v)

    => ((0 0) (1 0) (2 0) (0 2) (1 2) (2 2))

Note: This function makes no guarantees about the order in which the
grid points will be covered.  It only guarantees that they all will be
covered."

  (let ((from (or from (mapcar (constantly 0) to)))
        (by (or by (mapcar (constantly 1) to))))

    (check-type to (list-of+ real))
    (check-type from (list-of+ real))
    (check-type by (list-of+ real))

    (assert (= (length to) (length from)))
    (assert (= (length to) (length by)))

    (assert (every #'<= from to) (from to)
            "FROM (~A) cannot be more than TO (~A)" from to)
    (assert (every #'plusp by) (by)
            "BY increments must be positive (~A)" by)

    (let (last
          (got t))
      (lambda ()
        (cond
          ((and got (null last))
           (setf last from))

          (got
           (multiple-value-setq (last got)
             (grid-increment last to from by))))
        (values last got)))))
