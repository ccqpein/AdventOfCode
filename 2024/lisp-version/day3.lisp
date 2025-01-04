(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day3.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day3_demo.input"))

(defun day3 (&optional (input *input*))
  (let ((all (loop for line in input
                   append (cl-ppcre:all-matches-as-strings "mul\\(\\d+,\\d+\\)" line))))
    (format t "all length: ~a~%" (length all))
    (loop for a in all
          sum (str:match a
                (("mul" "\\(" x "," y "\\)") (* (parse-integer x) (parse-integer y)))))))

(defun day3-2 (&optional (input *input*))
  (let ((all (loop for line in input
                   append (cl-ppcre:all-matches-as-strings "(mul\\(\\d+,\\d+\\)|do\\(\\)|don\\'t\\(\\))" line))))
    (format t "all length: ~a~%" (length all))
    (do* ((rest all (cdr rest))
          (this (car rest) (car rest))
          (flag t)
          (result 0))
         ((not this) result)
      
      (cond ((string= this "don't()")
             (setf flag nil))
            ((string= this "do()")
             (setf flag t))
            (flag
             (str:match this
               (("mul" "\\(" x "," y "\\)") (incf result (* (parse-integer x) (parse-integer y))))))))))
