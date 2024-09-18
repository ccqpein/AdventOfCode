(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day6.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day6_demo.input"))

(defun max-value-key (table)
  (let ((max-v (apply #'max (alexandria:hash-table-values table))))
    (loop for char being each hash-key of table
            using (hash-value n)
          when (= n max-v)
            return char)))

(defun least-value-key (table)
  (let ((max-v (apply #'min (alexandria:hash-table-values table))))
    (loop for char being each hash-key of table
            using (hash-value n)
          when (= n max-v)
            return char)))

(defun day6 (&key part2 (input *input*))
  (loop
    with bucket =
                (loop repeat (length (first input)) collect (make-hash-table :test 'equal))
    for line in input
    do (loop with chars = (concatenate 'list line)
             for ind from 0 below (length chars)
             do (incf (gethash (nth ind chars) (nth ind bucket) 0)))
    finally (return-from day6
              (concatenate 'string
                           (loop for table in bucket
                                 collect (if part2
                                             (least-value-key table)
                                             (max-value-key table)))))))
