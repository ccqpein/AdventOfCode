(defparameter *stack*
  '(("Q" "H" "C" "T" "N" "S" "V" "B")
	("G" "B" "D" "W")
	("B" "Q" "S" "T" "R" "W" "F")
	("N" "D" "J" "Z" "S" "W" "G" "L")
	("F" "V" "D" "P" "M")
	("J" "W" "F")
	("V" "J" "B" "Q" "N" "L")
	("N" "S" "Q" "J" "C" "R" "T" "G")
	("M" "D" "W" "C" "Q" "S" "J")))

(defparameter *stack-demo*
  '(("N" "Z")
	("D" "C" "M")
	("P")))

(defun day5 (part)
  (let ((input (read-file-by-line "../inputs/day5.input"))
		(copy-stack (copy-seq *stack*)))
	(loop for line in input
		  for commands = (str:split " " line)
		  for num = (parse-integer (nth 1 commands))
		  and ff = (1- (parse-integer (nth 3 commands)))
		  and tt = (1- (parse-integer (nth 5 commands)))
		  
		  do (setf (nth tt copy-stack)
				   (append (if (= 1 part)
							   (reverse (subseq (nth ff copy-stack) 0 num))
							   (subseq (nth ff copy-stack) 0 num))
						   (nth tt copy-stack))
				   (nth ff copy-stack) (subseq (nth ff copy-stack) num))
		  )
	(str:join "" (mapcar #'car copy-stack))
	))
