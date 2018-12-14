(load "./general.lisp")

(defun day1 (filepath)
  (let ((sum 0))
	(with-open-file (stream filepath)
	  (do ((this (read stream nil) (read stream nil)))
		  ((not this) (print sum))
		(setf sum (+ sum (eval this)))))))

(defun day1-part2 (filepath sum results)
  (with-open-file (stream filepath)
	(do* ((this (read stream nil) (read stream nil))
		  )
		 ((not this) (return (day1-part2 filepath sum results)))
	  (setf sum (+ sum (eval this)))
	  (if (member sum results)
		  (return sum))
	  (setf results (cons sum results))
	  )))

;;(day1 filepath)
;;(day1-part2 filepath 0 '())

;;this is much quicker than first version
(defun day1-part2-2 (filepath)
  (let ((cache (mapcar #'(lambda (c)
						   (multiple-value-bind (a b)
							   (read-from-string c)
							 a))
					   (read-all-input filepath)))
		(sum 0))
	(do* ((rest cache (if (= 1 (length rest)) cache (cdr rest)))
		 (this (car rest) (car rest))
		 (result-table (make-hash-table))
		 (result nil))
		(result (return sum))
	  (setf sum (+ sum this))
	  (multiple-value-bind (v b)
		  (gethash sum result-table)
		(if b
			(setf result t)
			(setf (gethash sum result-table) '())))
	  )
	;;cache
	))
