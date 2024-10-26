(load "../../tools/tools.lisp")

(defparameter *input*
  "^.^^^..^^...^.^..^^^^^.....^...^^^..^^^^.^^.^^^^^^^^.^^.^^^^...^^...^^^^.^.^..^^..^..^.^^.^.^.......")

(defun one-line-traps (previous-line)
  (loop for (a b c) on (append '(#\.) previous-line '(#\.))
        ;;do (format t "~a ~a ~a~%" a b c)
        while (not (member nil (list a b c)))
        collect (cond ((equal '(#\^ #\^ #\.) (list a b c))
                       #\^)
                      ((equal '(#\. #\^ #\^) (list a b c))
                       #\^)
                      ((equal '(#\. #\. #\^) (list a b c))
                       #\^)
                      ((equal '(#\^ #\. #\.) (list a b c))
                       #\^)
                      (t #\.))))

(defun day18 (steps &optional (input *input*))
  (reduce (lambda (sum l)
            (+ sum (count #\. l)))
          (loop with line = (concatenate 'list input)
                repeat steps
                collect line into result
                do (setf line (one-line-traps line))
                finally (return result))
          :initial-value 0))
