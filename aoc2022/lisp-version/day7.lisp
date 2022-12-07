(load "../../tools/tools.lisp")

(defun day7 (part)
  (let ((input (read-file-by-line "../inputs/day7.input"))
		(tree '())
		(table (make-hash-table :test 'equal)))
	
	(setf (gethash "/" table) (make-hash-table :test 'equal))
	
	(dolist (line input)
	  (let ((split-line (str:split " " line)))
		(cond ((string= "$" (car split-line))
			   (cond ((string= "cd" (cadr split-line))
					  (setf tree (cd (caddr split-line) tree)))
					 ))
			  ((string= "dir" (car split-line))
			   (give-dir-table tree (cadr split-line) table))
			  (t (give-value-table tree split-line table)))))

	(let ((record (make-hash-table :test 'equal)))
	  (count-table table record '())
	  (if (= 1 part)
		  (part1 record)
		  (part2 record)))
	))

(defun cd (dir tree)
  (if (string= dir "..")
	  (cdr tree)
	  (push dir tree)))

(defun give-dir-table (tree dir-name table)
  (loop
	with this-t = table
	for d in (reverse tree)
	do (setf this-t (gethash d this-t))
	finally (setf (gethash dir-name this-t) (make-hash-table :test 'equal))
	)
  )

(defun give-value-table (tree rest-output table)
  (loop
	with this-t = table
	for d in (reverse tree)
	do (setf this-t (gethash d this-t))
	finally (setf (gethash (cadr rest-output) this-t) (parse-integer (car rest-output))))
  )

(defun count-table (table result prefix)
  (loop
	with total = 0
	for k being each hash-key 
	  using (hash-value v) in table

	if (hash-table-p v)
	  do (incf total (count-table v result (append prefix (list k))))
	else
	  do (incf total v)
	finally (progn
			  (setf (gethash (str:join "->" prefix) result) total)
			  (return total))
	))

(defun part1 (table)
  (loop
	for k being each hash-key 
	  using (hash-value v) in table
	if (< v 100000)
	  sum v into result
	end
	finally (return result))
  )

(defun part2 (table)
  (let* ((all (gethash "/" table))
		 (need (- 30000000 (- 70000000 all))))
	(find-if (lambda (v) (> v need))
			 (sort
			  (loop for v being each hash-value in table
					collect v)
			  #'<))))
