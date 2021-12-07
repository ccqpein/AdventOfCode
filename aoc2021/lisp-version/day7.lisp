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
      minimize (apply #'+ (mapcar (lambda (e) (step-consume (abs (- e d)))) content))
      )))

;;; faster implement below
(defun part1-v2 (path)
  (let* ((content (sort (mapcar #'parse-integer
                                (str:split #\, (car (read-file-by-line path))))
                        #'<))
         (median-v (floor (/ (+ (nth (floor (/ (1- (length content)) 2)) content)
                                (nth (floor (/ (length content) 2)) content))
                             2))))
    (loop
      for d in content
      sum (abs (- d median-v))
      )))

(defun part2-v2 (path)
  (let* ((content (sort (mapcar #'parse-integer
                                (str:split #\, (car (read-file-by-line path))))
                        #'<))
         (mean-v (floor (/ (apply #'+ content)
                           (length content)))))
    (loop
      for m from (1- mean-v) to (1+ mean-v)
      minimize (apply #'+
                      (mapcar (lambda (v) (let ((diff (abs (- m v))))
                                            (floor (/ (* diff (1+ diff)) 2))))
                              content)))))
