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
;;; time = b, -1 = a, -distance = c;
;;; (floor x2) - (floor x1)

(defun formula (a b c)
  (let (x1 x2)
    (let ((part (expt (- (expt b 2) (* 4 a c)) 1/2))
          )
      (setf x1 (/ (- part b) (* 2 a))
            x2 (/ (- (+ b part)) (* 2 a))))
    (abs (- (floor x1) (floor x2)))
    ))

;;; example
;;; (formula -1 71530 -940200)
