(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day15.input"))
(defparameter *demo-input* (read-file-by-line "../inputs/day15_demo.input"))

(defun parse-line (line)
  (let ((s-line (str:split " " line)))
	(let ((s-x (parse-integer (subseq (nth 2 s-line) 2 (1- (length (nth 2 s-line))))))
		  (s-y (parse-integer (subseq (nth 3 s-line) 2 (1- (length (nth 3 s-line))))))
		  (b-x (parse-integer (subseq (nth 8 s-line) 2 (1- (length (nth 8 s-line))))))
		  (b-y (parse-integer (subseq (nth 9 s-line) 2))))
	  (list s-x s-y b-x b-y))))

(defun m-distance (s-x s-y b-x b-y)
  (+ (abs (- s-x b-x)) (abs (- s-y b-y))))

(defun if-line-in-range (s-x s-y b-x b-y line-num)
  (let ((dis (m-distance s-x s-y b-x b-y)))
	(> (+ s-y dis) line-num (- s-y dis))
	))

(defun if-col-in-range (s-x s-y dis row-num col-num)
  (if (<= (m-distance s-x s-y col-num row-num) dis)
	  (list t
			(if (< (- col-num s-x) 0)
				(* 2 (abs (- col-num s-x)))
				(- dis (- col-num s-x) (abs (- row-num s-y)))))
	  (list nil 0)))

(defun count-in-given-line (s-x s-y b-x b-y line-num)
  (if (if-line-in-range s-x s-y b-x b-y line-num)
	  (let* ((distance (m-distance s-x s-y b-x b-y))
			 (y-offset (abs (- line-num s-y)))
			 (x-offset (- distance y-offset)))
		(loop for x from (- x-offset) to x-offset
			  collect (list (+ x s-x) line-num)))))

(defun count-un-nil (table)
  (loop for v being the hash-value of table
		when v count 1))

(defun part1 (line-num &optional (input *input*))
  (loop
	with set = (make-hash-table :test 'equal)
	for line in input
	for (s-x s-y b-x b-y) = (parse-line line)
	collect (list s-x s-y) into all-s
	collect (list b-x b-y) into all-b
	when (if-line-in-range s-x s-y b-x b-y line-num)
	  do (loop for p in (count-in-given-line s-x s-y b-x b-y line-num)
			   do (setf (gethash p set) t))
	finally (progn
			  (loop for s in all-s do (setf (gethash s set) nil))
			  (loop for b in all-b do (setf (gethash b set) nil))
			  (return (count-un-nil set)))))

(defun part2 (edge &optional (input *input*))
  (let ((all-s
		  (loop for line in input
				for (s-x s-y b-x b-y) = (parse-line line)
				collect (list (list s-x s-y)
							  (m-distance s-x s-y b-x b-y)))))
	(loop for row from 0 to edge
		  do (loop for col from 0 to edge
				   do (loop for ((s-x s-y) dis) in all-s
							for (b offset) = (if-col-in-range s-x s-y dis row col)
							when b
							  do (incf col offset)
							  and do (return)
							finally (return-from part2 (+ (* 4000000 col) row)))))))

