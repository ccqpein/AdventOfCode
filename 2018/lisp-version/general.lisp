(ql:quickload :cl-ppcre)

(defun read-all-input (filepath)
  (with-open-file (stream filepath)
	(do ((this-line (read-line stream nil) (read-line stream nil))
		 (cache '()))
		((not this-line)
		 cache)
	  (setf cache (append cache (list this-line))))))


(defun read-all-input-together (filepath)
  (with-open-file (stream filepath)
	(do ((this-char (read-char stream nil) (read-char stream nil))
		 (cache '()))
		((not this-char)
		 cache)
	  (setf cache (append cache (list this-char))))))


(defun read-coordinate (str)
  "str = \"aaa, bbb\""
  (mapcar #'(lambda (e) (parse-integer e)) (cl-ppcre:split ", " str)))
