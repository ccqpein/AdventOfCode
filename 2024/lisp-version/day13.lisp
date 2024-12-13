(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day13.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day13_demo.input"))

(defun parse-input (input)
  (loop for (a b target) in (split-sequence:split-sequence-if (lambda (l) (string= l "")) input)
        collect (list (str:match a
                        (("Button A: X\\+" x ", Y\\+" y) (list (parse-integer x) (parse-integer y))))
                      (str:match b
                        (("Button B: X\\+" x ", Y\\+" y) (list (parse-integer x) (parse-integer y))))
                      (str:match target
                        (("Prize: X=" x ", Y=" y) (list (parse-integer x) (parse-integer y)))))))

(defun bf (a b target)
  (loop for i from 0 to 100
        append (loop for j from 0 to 100
                     when (and (= (+ (* i (first a)) (* j (first b))) (first target))
                               (= (+ (* i (second a)) (* j (second b))) (second target)))
                       collect (list i j))))

(defun min-tokens (list-of-button)
  (if (null list-of-button) (return-from min-tokens 0))
  (loop for (i j) in list-of-button
        minimize (+ (* 3 i) j)))

(defun day13 (&optional (input *input*))
  (let ((input (parse-input input)))
    (loop for (a b target) in input
          sum (min-tokens (bf a b target)))))

(defun equations (a b target)
  (let ((ax (first a))
        (ay (second a))
        (bx (first b))
        (by (second b))
        (x (first target))
        (y (second target))
        i
        j)
    (setf i (/ (- (* x by) (* y bx))
               (- (* ax by) (* ay bx))))
    (setf j (/ (- (* x ay) (* y ax))
               (- (* bx ay) (* by ax))))
    (list i j)))

;;(equations '(94 34) '(22 67) '(10000000008400 10000000005400))
;;(equations '(26 66) '(67 21) '(10000000012748 10000000012176))

(defun day13-2 (&optional part2 (input *input*))
  (let ((input (parse-input input)))
    (loop for (a b target) in input
          for (i j) = (equations
                       a b
                       (mapcar (lambda (n)
                                 (+ (if part2
                                        10000000000000
                                        0)
                                    n))
                               target))
          ;;do (format t "i: ~a, j: ~a~%" i j)
          when (and (typep i 'integer)
                    (typep j 'integer))
            sum (+ (* 3 i) j))))
