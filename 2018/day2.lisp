(defun day2 (filepath)
  (with-open-file (stream filepath)
	(do ((this-line (read-line stream nil) (read-line stream nil))
		 (result-tuple (cons 0 0)))

		((not this-line)
		 (* (car result-tuple) (cdr result-tuple)))

	  ;;(print (count-number this-line))
	  (destructuring-bind (two? three?)
		  (count-number this-line)
		(if two? (setf (car result-tuple) (+ 1 (car result-tuple))))
		(if three? (setf (cdr result-tuple) (+ 1 (cdr result-tuple)))))
	  )))


(defun day2-part2 (filepath)
  (let ((cache '()))
	(with-open-file (stream filepath)
	  (do ((this-line (read-line stream nil) (read-line stream nil)))
		  ((not this-line))
		(setf cache (cons this-line cache))))

	;;(print cache)
	(do* ((content-set cache (cdr content-set))
		  (match-set (cdr content-set) (cdr content-set))
		  (flag nil))
		 
		 ((or (eql nil match-set)
			  flag)
		  flag)

	  (loop
		 with this-line = (car content-set)
		 for line in match-set
		 do (if (two-string-common this-line line)
				(progn (setf flag (list this-line line))
					   (return))))
	  )))


(defun count-number (str)
  (let ((table (make-hash-table))
		(tuple-of-2and3 (list nil nil)))
	(loop
	   for c across str
	   for (a b) = (multiple-value-bind (a b) (gethash c table) (list a b))
	   do (if (not b)
			  (setf (gethash c table) 1)
			  (setf (gethash c table) (+ 1 (gethash c table)))))
	
	(loop
	   for k being each hash-key in table
	   using (hash-value v)
	   do (cond ((= v 2)
				 (setf (car tuple-of-2and3) t))
				((= v 3)
				 (setf (cadr tuple-of-2and3) t))))
	
	tuple-of-2and3))


(defun two-string-common (str1 str2)
  (loop
	 with flag = 0
	 for c1 across str1
	 for c2 across str2
	 ;;do (progn (print c1) (print c2))
	 do (if (not (eql c1 c2))
			(if (>= flag 1)
				(return nil)
				(setf flag 1)))
	 finally (if (= flag 1)
				 (return t)
				 (return nil))))
