;;;; criterion.lisp

(in-package #:grid-generators-tests)

(nst:def-criterion (generates (expect) (generator))
  (let* ((got (loop :for v := (funcall generator)
                :while v
                 :collecting v))
         (diff1 (set-difference expect got :test #'equalp))
         (diff2 (set-difference got expect :test #'equalp)))
    (cond
      ((and diff1 diff2)
       (nst:make-failure-report :format "Expected to get ~A but did not, and did not expected to get ~A but did."
                                :args (list diff1 diff2)))

      (diff1
       (nst:make-failure-report :format "Expected to get ~A but did not."
                                :args (list diff1)))

      (diff2
       (nst:make-failure-report :format "Did not expect to get ~A but did."
                                :args (list diff2)))

      (t
       (nst:make-success-report)))))

(nst:def-criterion (monotonic (&key (test #'<=)
                                    (key #'identity))
                              (generator))
  (let* ((generator (eval generator))
         (got (loop :for v := (funcall generator)
                 :while v
                 :collecting v))
         (keys (mapcar (eval key) got)))
    (if (every test keys (rest keys))
        (nst:make-success-report)
        (nst:make-failure-report :format "Not monotonic: ~A"
                                 :args got))))
