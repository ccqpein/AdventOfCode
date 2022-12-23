(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day22.input"))
(defparameter *demo-input* (read-file-by-line "../inputs/day22_demo.input"))

(defun parse-input (&optional (input *input*))
  (let (rr)
   (values
	(loop
	  with map = (make-hash-table :test 'equal)
	  for r from 0
	  when (string= (nth r input) "")
		do (setf rr r)
		and return map
	  do (loop
		   with line = (concatenate 'list (nth r input))
		   for c from 0
		   when (string/= " " (nth c line))
			 do (setf (gethash r map)
					  (cons c (subseq line c)))
			 and return nil)
	  )

	(parse-codes (nth (1+ rr) input))
	)))

(defun parse-codes (ss)
  (do ((rest (concatenate 'list ss) (cdr rest))
	   cache
	   ll)
	  ((not rest)
	   (if cache
		   (push (parse-integer (str:join "" (reverse cache))) ll))
	   (reverse ll))
	(if (member (car rest) '(#\L #\R))
		(progn
		  (push (parse-integer (str:join "" (reverse cache))) ll)
		  (push (string (car rest)) ll)
		  (setf cache nil))
		(push (string (car rest)) cache)
		)
	))

(defun move (map step turn current)
  (let (new-direction)
	(setf new-direction
		  (if (string= turn "R")
			  (cond ((string= ">" (car current)) "v")
					((string= "v" (car current)) "<")
					((string= "<" (car current)) "^")
					((string= "^" (car current)) ">"))
			  
			  (cond ((string= ">" (car current)) "^")
					((string= "v" (car current)) ">")
					((string= "<" (car current)) "v")
					((string= "^" (car current)) "<"))))

	(cond ((string= new-direction "^")
		   (move-vert map (* -1 step) (cdr current)))
		  ((string= new-direction ">")
		   (move-hori map step (cdr current)))
		  ((string= new-direction "v")
		   (move-vert map step (cdr current)))
		  ((string= new-direction "<")
		   (move-hori map (* -1 step) (cdr current))))))

(defun get-v (map pos)
  ;;(format t "pos ~a~%" pos)
  (let* ((rr (mod (car pos) (length (alexandria:hash-table-keys map))))
		 (line (gethash rr map))
		 )
	(if (<= 0 (- (cadr pos) (car line)) (length line))
		(nth (- (cadr pos) (car line)) line)
		nil)))

(defun find-the-first-row (map col)
  (loop for r from 0
		for line = (gethash r map)
		if (< 0 (- col (car line)) (length line))
		  return r))

(defun find-the-last-row (map col)
  (loop
	with row-edge = (length (alexandria:hash-table-keys map))
	for r from (1- row-edge) downto 0
	for line = (gethash r map)
	if (< 0 (- col (car line)) (length line))
	  return r))

(defun move-vert (map step pos)
  (if (>= step 0)
	  (loop
		with r = (car pos)
		for i from 0 to step
		for v = (get-v map (list r (cadr pos)))
		if (not v)
		  do (setf r (find-the-first-row map (cadr pos))
				   v (get-v map (list r (cadr pos))))
	   	if (char= v #\#)
		  return (list r (cadr pos))
		do (incf r)
		finally (return (list (1- r) (cadr pos)))
		)

	  (loop
		with r = (car pos)
		for i from 0 to (abs step)
		for v = (get-v map (list r (cadr pos)))
		do (print v)
		if (not v)
		  do (setf r (find-the-last-row map (cadr pos))
				   v (get-v map (list r (cadr pos))))
	   	if (char= v #\#)
		  return (list r (cadr pos))
		do (decf r)
		finally (return (list (1+ r) (cadr pos)))
		)
	  )
  )

(defun move-hori (map step pos))
