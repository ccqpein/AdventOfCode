(defun read-all-input (filepath)
  (with-open-file (stream filepath)
	(do ((this-line (read-line stream nil) (read-line stream nil))
		 (cache '()))
		((not this-line)
		 cache)
	  (setf cache (append cache (list this-line))))))

