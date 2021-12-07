(load "../../2020/tools.lisp")

(defun part1 (path)
  (let ((content (sort (mapcar #'parse-integer
                               (str:split #\, (car (read-file-by-line path))))
                       #'<)))
    (loop
      for d from 0 to (car (last content))
      minimize (apply #'+ (mapcar (lambda (e) (abs (- e d))) content)) into result
      finally (return result)
      )))

(defun step-consume (step)
  (loop
    with init = 0
    repeat step
    sum (incf init)))

(defun part2 (path)
  (let ((content (sort (mapcar #'parse-integer
                               (str:split #\, (car (read-file-by-line path))))
                       #'<)))
    (loop
      for d from 0 to (car (last content))
      minimize (apply #'+ (mapcar (lambda (e) (step-consume (abs (- e d)))) content)) into result
      finally (return result)
      )))
