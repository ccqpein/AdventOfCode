(load "../../tools/tools.lisp")

;;(defparameter *input* (read-file-by-line "../inputs/day12.input"))
;;(defparameter *alphabet* (mapcar #'string (concatenate 'list "abcdefghijklmnopqrstuvwxyz")))

#|
(defun day12 ()
  (let (
		;;(*input* (read-file-by-line "../inputs/day12_demo.input"))
		;;(*alphabet* '())
		(table (make-hash-table :test 'equal))
		)
	(print (one-step (parse-input *input*) *alphabet* 20 0 '() table))
	;;(print (one-step (parse-input *input*) (cddr *alphabet*) 20 113 '() table))
	(loop for k being the hash-keys 
			using (hash-value v) of table
		  do (format t "~a: ~a~%" k v))
	))

(defun demo ()
  (let ((*input* (read-file-by-line "../inputs/day12_demo.input"))
		;;(*alphabet* '())
		(table (make-hash-table :test 'equal)))
	;;(print (one-step (parse-input *input*) (cddr *alphabet*) 1 2 '() table))
	(print (one-step (parse-input *input*) *alphabet* 0 0 '() table))
	(loop for k being the hash-keys 
			using (hash-value v) of table
		  do (format t "~a: ~a~%" k v))
  ))

(defun find-S (map)
  (loop for r from 0 to (length map)
		do (loop for c from 0 to (length (car map))
				 if (string= (nth c (nth r map)) "S")
				   do (return-from find-S (list r c)))))

(defun find-E (map)
  (loop for r from 0 to (length map)
		do (loop for c from 0 to (length (car map))
				 if (string= (nth c (nth r map)) "E")
				   do (return-from find-E (list r c)))))

(defun get-map (map x y)
  (nth y (nth x map)))

(defun parse-input (input)
  (loop for line in input
		collect (mapcar #'string (concatenate 'list line))))

(defun one-step (map rest x y already table)
  ;;(format t "try get: ~a: ~a~%" (list x y) (gethash (list x y) table))
  (multiple-value-bind (v present)
	  (gethash (list x y) table)
	(if present
		v
		(let ((up (list (1- x) y))
			  (down (list (1+ x) y))
			  (left (list x (1- y)))
			  (right (list x (1+ y))))
		  (loop
			with result = '()
	  
			for op in (list up down left right)
			for (xx yy) = op

			when (and
				  (not (find-if (lambda (e) (< e 0)) op))
				  (not (member op already :test 'equal)))
		
			  do (cond
				   ((and (or (not rest) (string= "z" (car rest)))
						 (string= (nth yy (nth xx map)) "E"))
					(push 1 result)
					)
			 
				   ((string= (nth yy (nth xx map)) (car rest))
					(let ((vv (one-step map rest xx yy (cons (list x y) already) table)))
					  (if vv
						  (push (1+ vv) result))))
			 
				   ((string= (nth yy (nth xx map)) (cadr rest))
					(let ((vv (one-step map (cdr rest) xx yy (cons (list x y) already) table)))
					  (if vv
						  (push (1+ vv) result))))
				   )
			  ;;and do (print (list (list x y) op rest (nth yy (nth xx map)) already result))
		   
			finally (return
					  (multiple-value-bind (v p)
						  (gethash (list x y) table)
						(cond ((and (not result) p)
							   v)
								
							  ((and result p)
							   (cond ((not v)
									  (setf (gethash (list x y) table)
											(apply #'min result))
									  (apply #'min result))
									 (t  (setf (gethash (list x y) table)
											   (apply #'min (cons v result)))
										 (apply #'min (cons v result)))))
								
							  ((and result (not p))
							   (setf (gethash (list x y) table)
									 (apply #'min result))
							   (apply #'min result)
							   )
								
							  ((and (not result) (not p))
							   (setf (gethash (list x y) table) nil)
							   nil)))
					  )
			)))))
|#

(defparameter *input* (read-file-by-line "../inputs/day12.input"))

(defun parse-input (input)
  (let (start end map)
	(setf map
		  (loop for r from 0 below (length input)
				collect (loop
						  with line = (concatenate 'list (nth r input))
						  for c from 0 below (length (first input))
						  collect (char-code
								   (cond ((char= #\S (nth c line)) (setf start (cons r c)) #\a)
										 ((char= #\E (nth c line)) (setf end (cons r c)) #\z)
										 (t (nth c line)))))))
	(values start end map)))

(defun neighbours (map x y)
  (let ((visited (make-hash-set))
		(x-max (length map))
		(y-max (length (car map))))
	(loop for dx in '(-1 0 1 0)
		  for dy in '(0 1 0 -1)
		  for xx = (+ x dx)
		  for yy = (+ y dy)
		  if (<= 0 xx (1- x-max))
			if (<= 0 yy (1- y-max))
			  if (<= (nth-nest map (list xx yy))
					 (1+ (nth-nest map (list x y))))
				collect (cons xx yy))))

(defun handle (map start end)
  (let ((queue (make-queue))
		(visited (make-hash-table :test #'equal)))
	
	(pushq queue (cons start 0))
	(setf (gethash start visited) t)

	(loop while (not (emptyq queue))
		for (current . score) = (popq queue)
		do (if (equal current end)
			   (return score)
			   (loop for n in (neighbours map (car current) (cdr current))
					 if (null (gethash n visited))
					   do (progn
							(setf (gethash n visited) t)
							(pushq queue (cons n (1+ score)))))))
	))

(defun handle2 (map end)
  (let (all-As)
	(setf all-As
		  (loop for r from 0 below (length map)
				append (loop for c from 0 below (length (first map))
							 if (= 97 (nth-nest map (list r c)))
							   collect (cons r c))))
	
	(apply #'min
		   (remove nil
				   (loop for aa in all-As
						 collect (handle map aa end))))
	))

(defun day12 (&optional part)
  (multiple-value-bind (start end map)
	  (parse-input *input*)
	(if (= 1 part)
		(handle map start end)
		(handle2 map end))))

;; simple queue definition
;; backed by a list. represented by 2 pointers: first cons cell, last cons cell.
(defun make-queue () (list nil nil))

(defun pushq (q elem)
  ;; push it & update the last cons cell.
  (setf (cdr q)
	(setf (cddr q) (cons elem nil)))
  (when (emptyq q)
    ;; fix the head pointer.
    (setf (car q) (cdr q)))
  q)

(defun popq (q)
  (prog1 (caar q)
    (setf (car q) (cdar q))))

(defun emptyq (q) (null (car q)))
