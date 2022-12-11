(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day10.input"))

(defun day10 ()
  (let* (;;(*input* (read-file-by-line "../inputs/day10_demo.input"))
		 (parse-input (loop for line in *input*
							for (comm num) = (str:split " " line)
							if (string= comm "addx")
							  append (list 0 (parse-integer num)) into result
							else
							  append '(0) into result
							end
							finally (return result))))
	(loop
	  with flag = 20 and count = 1
	  for i from 0 below (length parse-input)	  
	  if (= (+ 1 i) flag)
		sum (* count flag) into result
		and do (incf flag 40)
	  end 
	  do (incf count (nth i parse-input))
	  finally (return-from day10 result)
	  )))

(defun day10-part2 ()
  (let* (;;(*input* (read-file-by-line "../inputs/day10_demo.input"))
		 (parse-input (loop for line in *input*
							for (comm num) = (str:split " " line)
							if (string= comm "addx")
							  append (list 0 (parse-integer num)) into result
							else
							  append '(0) into result
							end
							finally (return result))))

	(loop
	  with sprite = '(0 1 2)
	  for i from 0 below (length parse-input)
	  for new-sprite-mid = (+ (nth i parse-input) (cadr sprite))

	  if (member (mod i 40) sprite)
		collect "#" into result
	  else
		collect "." into result
	  end
	  
	  do (setf sprite (list (1- new-sprite-mid)
							new-sprite-mid
							(1+ new-sprite-mid)))
	  finally (format
			   t
			   "~{~a~%~}"
			   (mapcar (lambda (line)
						 (apply #'str:concat line))
					   (chunk-list result 40)))
	  )))
