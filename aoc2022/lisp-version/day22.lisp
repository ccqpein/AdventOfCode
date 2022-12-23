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
  (let (new-direction new-pos)
	(setf new-direction
		  (cond ((string= turn "R")
				 (cond ((string= ">" (car current)) "v")
					   ((string= "v" (car current)) "<")
					   ((string= "<" (car current)) "^")
					   ((string= "^" (car current)) ">")))
			  
				((string= turn "L")
				 (cond ((string= ">" (car current)) "^")
					   ((string= "v" (car current)) ">")
					   ((string= "<" (car current)) "v")
					   ((string= "^" (car current)) "<")))
				(t (car current))))

	(setf new-pos
		  (cond ((string= new-direction "^")
				 (move-vert map (* -1 step) (cdr current)))
				((string= new-direction ">")
				 (move-hori map step (cdr current)))
				((string= new-direction "v")
				 (move-vert map step (cdr current)))
				((string= new-direction "<")
				 (move-hori map (* -1 step) (cdr current)))))

	(cons new-direction new-pos)))

(defun get-v (map pos)
  ;;(format t "pos ~a~%" pos)
  (let* ((rr (mod (car pos) (length (alexandria:hash-table-keys map))))
		 (line (gethash rr map))
		 )
	(if (<= 0 (- (cadr pos) (car line)) (- (length line) 2))
		(nth (1+ (- (cadr pos) (car line))) line)
		nil)))

(defun find-the-first-row (map col)
  (loop for r from 0
		for line = (gethash r map)
		if (< 0 (- col (car line)) (1- (length line)))
		  return r))

(defun find-the-last-row (map col)
  (loop
	with row-edge = (length (alexandria:hash-table-keys map))
	for r from (1- row-edge) downto 0
	for line = (gethash r map)
	if (< 0 (- col (car line)) (1- (length line)))
	  return r))

(defun move-vert (map step pos)
  (if (>= step 0)
	  (loop
		with r = (car pos)
		and new-r = (1+ (car pos))
		
		for i from 1 to step
		for v = (get-v map (list new-r (cadr pos)))
		if (not v)
		  do (setf new-r (find-the-first-row map (cadr pos))
				   v (get-v map (list new-r (cadr pos))))
	   	if (char= v #\#)
		  return (list r (cadr pos))
		do (setf r new-r
				 new-r (1+ new-r))
		finally (return (list r (cadr pos)))
		)

	  (loop
		with r = (car pos)
		and new-r = (1- (car pos))
		
		for i from 1 to (abs step)
		for v = (get-v map (list new-r (cadr pos)))
		;;do (print v)
		if (not v)
		  do (setf new-r (find-the-last-row map (cadr pos))
				   v (get-v map (list new-r (cadr pos))))
		;;do (format t "new-r: ~a~%" new-r)
	   	if (char= v #\#)
		  return (list r (cadr pos))
		do (setf r new-r
				 new-r (1- new-r))
		finally (return (list r (cadr pos)))
		)
	  )
  )

(defun find-the-leftest-col (map r)
  (car (gethash r map))
  )

(defun find-the-rightest-col (map r)
  (+ (car (gethash r map)) (length (gethash r map)) -2)
  )

(defun move-hori (map step pos)
  (if (>= step 0)
	  (loop
		with c = (cadr pos)
		and new-c = (1+ (cadr pos))
		
		for i from 1 to step
		for v = (get-v map (list (car pos) new-c))
		if (not v)
		  do (setf new-c (find-the-leftest-col map (car pos))
				   v (get-v map (list (car pos) new-c)))
		if (char= v #\#)
		  return (list (car pos) c)
		do (setf c new-c
				 new-c (1+ new-c))
		finally (return (list (car pos) c))
		)
	  (loop
		with c = (cadr pos)
		and new-c = (1- (cadr pos))
		for i from 1 to (abs step)
		for v = (get-v map (list (car pos) new-c))
		;;do (format t "r: ~a, c: ~a, v: ~a~%" (car pos) c v)
		if (not v)
		  do (setf new-c (find-the-rightest-col map (car pos))
				   v (get-v map (list (car pos) new-c)))
		if (char= v #\#)
		  return (list (car pos) c)
		do (setf c new-c
				 new-c (1- new-c))
		finally (return (list (car pos) c))
		))
  )

(defun part1 (&optional (input *input*))
  (let (map codes pos)
	(multiple-value-setq (map codes) (parse-input input))

	(setf pos (list ">" 0 (car (gethash 0 map))))
	(format t "pos: ~a~%" pos)
	(setf pos (move map (car codes) "" pos)
		  codes (cdr codes))
	(format t "pos: ~a~%" pos)
	
	(do ((codes codes)
		 (direction (car codes) (car codes))
		 (step (cadr codes) (cadr codes)))
		((not step) pos)
	  (format t "step: ~a, direction: ~a~%"
			  step direction)
	  (setf codes (cddr codes))

	  (setf pos (move map step direction pos))
	  (format t "pos: ~a~%" pos)
	  )
	
	(+ (* 1000 (1+ (second pos)))
	   (* 4 (1+ (third pos)))
	   (cond ((string= ">" (car pos))
			  0)
			 ((string= "v" (car pos))
			  1)
			 ((string= "<" (car pos))
			  2)
			 ((string= "^" (car pos))
			  3)))
	))

(let (map codes)
  (multiple-value-setq (map codes) (parse-input *input*))
  (move-hori map -32 '(2 70))
  )
