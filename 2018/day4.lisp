;;;
;;; I rewrite with python
;;;
(load "./general.lisp")

(defun parse-line (str)
  (loop
	 with result = '()
	 with cache = '()
	 for c across str
	 do (case c
		  ((#\  #\[ #\]) (setf result (append result (list cache))
							   cache '()))
		  (otherwise (setf cache (append cache (list c)))))
	 finally (setf result (mapcar (lambda (x) (concatenate 'string x))
								  (remove nil (append result (list cache)))))
	 finally (return result)))

(defun time-parse (str)
  (loop
	 with cache = '()
	 with result = '()
	 for c across str
	 do (if (not (eql #\: c))
			(setf cache (append cache (list c))
				  )
			(setf result (append result (list cache))
				  cache '()))
	 finally (setf result (mapcar (lambda (x) (concatenate 'string x))
								  (append result (list cache))))
	 finally (return result)))

(defun date2num (str)
  (loop
	 with cache = '()
	 with result = '()
	 for c across str
	 do (if (not (eql #\- c))
			(setf cache (append cache (list c)))
			(setf result (append result cache)
				  cache '()))
	 finally (setf result (concatenate 'string (append result cache)))
	 finally (return (parse-integer result))))

(defun exchge2minute (str)
  (let ((time (time-parse str))
		(result 0))
	(if (equal "00" (car time))
		(setf result (+ 60 (parse-integer (cadr time)) result))
		(setf result (+ (parse-integer (cadr time)) result)))))

(defun one-day-after (str)
  )

(defun day4 (filepath)
  (let ((cache (loop
				  for line in (read-all-input filepath)
				  collect (parse-line line)))
		(schedule (make-hash-table :test 'equal)) ;;date : guard
		(guard-work (make-hash-table :test 'equal)) ;; guard : startdate
		(timesheet (make-hash-table :test 'equal))
		(guard-sleep-time (make-hash-table :test 'equal))) ;; date : list of timestamp
	

	(loop
	   for line in cache
	    
	   do (cond ((equal "Guard" (nth 2 line))
				 (setf (gethash (nth 0 line) schedule) (nth 3 line)
					   (gethash (nth 3 line) guard-work '()) (list (nth 0 line) (time-parse (nth 1 line)))
					   ))
				(t
				 (setf (gethash (nth 0 line) timesheet '())
					   (cons (exchge2minute (nth 1 line)) (gethash (nth 0 line) timesheet))))))

	(loop
	   for g in being the hash-keys of guard-work
	   using (hash-value date)
	   do (if (equal (caadr date) "23")
			  ))
	
	(pprint schedule)
	(loop for k being the hash-keys of guard-work
	   using (hash-value v)
	   do (print k)
	   do (print v))
	(loop for k being the hash-keys of timesheet
	   using (hash-value v)
	   do (print k)
	   do (print v))))

