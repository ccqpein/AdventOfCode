(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day2.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day2_demo.input"))

(defun adj (ll)
  (loop with flag = (< (- (second ll) (first ll)) 0)
        for (a b) on ll
        when b
          do (cond (flag (if (not (>= 3 (- a b) 1)) (return-from adj nil)))
                   ((not flag) (if (not (>= 3 (- b a) 1)) (return-from adj nil))))
        finally (return t)))

(defun adj-2 (ll)
  (loop for ind from 0 below (length ll)
        for ll2 = (append (subseq ll 0 ind) (subseq ll (1+ ind)))
        when (adj ll2)
          return t
        finally (return nil)))

(defun day2 (part2 &optional (input *input*))
  (loop for line in input
        for ll = (mapcar #'parse-integer (str:words line))
        for ll2 = (sort (copy-list ll) #'<)
        for ll3 = (sort (copy-list ll) #'>)
        if (or (and (or (equal ll ll2) (equal ll ll3)) (adj ll))
               (if part2 (adj-2 ll) nil))
          count 1))
