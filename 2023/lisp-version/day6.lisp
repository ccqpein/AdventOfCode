(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day6.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day6_demo.input"))

(defun parse-input (input)
  (let ((times (cl-ppcre:all-matches-as-strings "\\d+" (car input)))
        (dis (cl-ppcre:all-matches-as-strings "\\d+" (cadr input))))
    (loop for tt in times
          for dd in dis
          collect (list (parse-integer tt) (parse-integer dd)))))

(defun day6 (input)
  (let ((input (parse-input input)))
    (apply #'*
           (loop for (tt dis) in input
                 collect (loop for speedup from 0 to tt
                               if (> (* speedup (- tt speedup)) dis)
                                 sum 1)))))

;;; part2:
;;; x is speed up time: (time - x) * x > distance
;;; (floor x2) - (floor x1)
