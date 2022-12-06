(load "../../tools/tools.lisp")

(defun day6 (part)
  (let ((input (concatenate 'list
							(car (read-file-by-line "../inputs/day6.input"))))
		(distinct (if (= 1 part) 4 14)))
	(loop
	  with start = 0
	  and count = 0
	  
	  for end = (+ distinct start)
	  if (all-diff (subseq input start end))
		do (return (+ distinct count))
	  else
		do (setf start (1+ start)
				 count (1+ count)))
	))

(defun all-diff (sl)
  (equal (remove-duplicates sl) sl))
