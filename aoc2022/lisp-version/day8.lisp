(load "../../tools/tools.lisp")

(defun day8 ()
  (let* ((input (read-file-by-line "../inputs/day8.input"))
		 (tree-map 
		   (loop for line in input
				 collect (mapcar (lambda (c)
								   (parse-integer (string c)))
								 (concatenate 'list line)))))
	
	(format t "part1: ~a~%" (part1 tree-map))
	(format t "part2: ~a~%" (part2 tree-map))
	))

(defun part1 (tree-map)
  (let ((set (make-hash-set)))
	(loop
	  for row in tree-map
	  for x from 0
	  do (loop with largest = -1
			   for i in row for y from 0
			   when (> i largest)
				 do (setf largest i)
				 and do (set-insert set (list x y))
			   end)
	  do (loop with largest = -1
			   for i in (reverse row)
			   for y from (1- (length row)) downto 0
			   when (> i largest)
				 do (setf largest i)
				 and do (set-insert set (list x y))
			   end))

	(loop for y from 0 below (length (car tree-map))
		  for col = (mapcar (lambda (row) (nth y row)) tree-map)
		  do (loop with largest = -1
				   for i in col for x from 0
				   when (> i largest)
					 do (setf largest i)
					 and do (set-insert set (list x y))
				   end)
		  do (loop with largest = -1
				   for i in (reverse col)
				   for x from (1- (length col)) downto 0
				   when (> i largest)
					 do (setf largest i)
					 and do (set-insert set (list x y))
				   end))
	(set-count set)))

(defun part2 (tree-map)
  (loop for x from 0 below (length tree-map)
		append (loop for y from 0 below (length (car tree-map))
					 collect (apply #'* (count-trees tree-map x y)))
		  into cache
		finally (return (apply #'max cache))))

(defun count-trees (tree-map x y)
  (let* ((col (mapcar (lambda (row) (nth y row)) tree-map))
		 (row (nth x tree-map))
		 
		 (up (reverse (subseq col 0 x)))
		 (down (subseq col (1+ x)))
		 (left (reverse (subseq row 0 y)))
		 (right (subseq row (1+ y)))
		 
		 (this-tree (nth y (nth x tree-map))))
	
	(loop for line in (list up down left right)
		  collect (let ((aa 0))
					(dolist (i line aa)
					  (if (>= i this-tree)
						  (return (1+ aa))
						  (incf aa)))))
	))

