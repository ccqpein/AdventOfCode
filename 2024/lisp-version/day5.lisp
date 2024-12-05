(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day5.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day5_demo.input"))

(defun parse-input (input)
  (let* ((s-input (split-sequence:split-sequence-if (lambda (w) (string= w "")) input))
         (rules (first s-input))
         (updates (second s-input))
         (graph (make-graph)))
    (loop for r in rules
          for (a b) = (str:split "|" r)
          do (insert-graph-node graph a b 1))
    (values graph updates)))

(defun c-front (this rest rule-graph)
  (if (null rest)
      t
      (every (lambda (r) (member r (get-all-nodes-of-id-without-weight rule-graph this) :test 'equal))
             rest)))

(defun check-updates (updates rule-graph &optional part2)
  (loop for u in updates
        for us = (str:split "," u)
        sum (loop for rest on us
                  unless (c-front (car rest) (cdr rest) rule-graph)
                    return (if part2
                               (parse-integer (nth (floor (/ (length us) 2))
                                                   (re-order-incorrectly rule-graph us)))
                               0)
                  finally (return
                            (if part2
                                0
                                (parse-integer (nth (floor (/ (length us) 2)) us)))))))

(defun re-order-incorrectly (graph update)
  (sort update (lambda (x y) (member y (get-all-nodes-of-id-without-weight graph x) :test 'equal))))

(defun day5 (&optional part2 (input *input*))
  (multiple-value-bind (graph updates)
      (parse-input input)
    (check-updates updates graph part2)))
