(defun day3 (filepath)
  (let ((cache (with-open-file (stream filepath)
				 (do ((this-line (read-line stream nil) (read-line stream nil))
					  (cache '()))
					 ((not this-line)
					  cache)
				   (setf cache (append cache (list this-line))))))
		(area (make-array '(2000 2000) :initial-element nil))
		(count 0)) ;each element is (t '(ids))

	(loop
	   for line in cache
	   do (let* ((parsed-line (parse-single-line line))
				 (from-left (multiple-value-bind (i len)
								(parse-integer (nth 1 parsed-line))
							  i))
				 (from-up (multiple-value-bind (i len)
								(parse-integer (nth 2 parsed-line))
							i))
				 (to-right (multiple-value-bind (i len)
								(parse-integer (nth 3 parsed-line))
							i))
				 (to-down (multiple-value-bind (i len)
								(parse-integer (nth 4 parsed-line))
							i))
				 )
			(loop
			   for left2right from from-left to (+ from-left to-right -1)
			   do (loop
					 for up2down from from-up to (+ from-up to-down -1)
					 do (if (not (aref area left2right up2down))
							(setf (aref area left2right up2down)
								  (list t (list (nth 0 parsed-line))))
							(progn
							  (if (< (length (cadr (aref area left2right up2down))) 2)
								  (incf count))
							  (setf (cadr (aref area left2right up2down))
								   (append (cadr (aref area left2right up2down))
										   (list (nth 0 parsed-line))))))))
			))
	;; (pprint area)
	(values area count)
	))

;;; regex should be easy
(defun parse-single-line (str)
  (with-input-from-string (stream str)
    (do ((result '())
		 (cache '())
		 (flag 'empty)
		 (c (read-char stream) (read-char stream nil)))
        ((not (characterp c))
		 (loop for cl in (append result (list cache))
			collect (char-list-2-string cl)))
	  ;;(print flag)
      (cond ((eql #\@ c)
			 (setf flag 'coo))

			((and (eql 'coo flag)
				  (not (eql #\  c))
				  )
			 (cond ((eql #\: c)
					(progn (setf result (append result (list cache)))
						   (setf cache '())))
				   ((eql #\x c)
					(progn (setf result (append result (list cache)))
						   (setf cache '())))
				   ((eql #\, c)
					(progn (setf result (append result (list cache)))
						   (setf cache '())))
				   (t (setf cache (append cache (list c))))))
			
			((and (eql #\  c) (not (eql flag 'coo)))
			 (setf flag 'empty)
			 (setf result (append result (list cache)))
			 (setf cache '()))
			
			((eql 'id flag)
			 (setf cache (append cache (list c))))
			
			((eql #\# c)
			 (setf flag 'id))))))

(defun char-list-2-string (l)
  (concatenate 'string l))

;;total is numbers of plan
(defun day3-part2 (area total)
  (let* ((total-list (loop for i from 1 to total collect i))
		 (array (make-array (length total-list) :initial-contents total-list)))
	(loop
	   for left2right from 0 to 1999
	   do (loop
			 for up2down from 0 to 1999
			 do
			   (let ((this (aref area left2right up2down)))
				 (if (> (length (cadr this)) 1)
					 (loop
						for i in (cadr this)
						for ii = (parse-integer i)
						do (setf (aref array (- ii 1)) nil)))))
		 )
	(loop
	   for now across array
	   do (if now
			  (print now)))))

;; (day3-part2 (day3 filepath) 1267)
