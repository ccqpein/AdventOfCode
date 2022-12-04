(load "../../tools/tools.lisp")

(defun day4 ()
  (let ((input (read-file-by-line "../inputs/day4.input")))
	(loop for line in input
		  for (a b) = (str:split "," line)
		  for (a0 a1) = (mapcar #'parse-integer (str:split "-" a))
		  for (b0 b1) = (mapcar #'parse-integer (str:split "-" b))
		  sum (if (or
				   (and (<= a0 b0) (>= a1 b1))
				   (and (<= b0 a0) (>= b1 a1)))
				  1
				  0))))

(defun day4-part2 ()
  (let ((input (read-file-by-line "../inputs/day4.input")))
	(loop for line in input
		  for (a b) = (str:split "," line)
		  for (a0 a1) = (mapcar #'parse-integer (str:split "-" a))
		  for (b0 b1) = (mapcar #'parse-integer (str:split "-" b))
		  sum (if (or
				   (and (< a0 b0) (< a1 b0))
				   (and (< b0 a0) (< b1 a0)))
				  0
				  1))))
