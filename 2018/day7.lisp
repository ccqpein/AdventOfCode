(load "./general.lisp")

(defun print-hashtable (table)
  (loop
	 for k being the hash-keys of table
	 using (hash-value v)
	 do (format t "~:c : ~a~%" k v)))

(defun parse-input (str)
  (let ((spilt-str (cl-ppcre:split " " str)))
	(cons (elt (nth 1 spilt-str) 0)
		  (elt (nth 7 spilt-str) 0))))


(defun init-table (pair before after)
  (setf (gethash (cdr pair) before) '()
		(gethash (car pair) after) '()))


(defun add-to-hashtable (pair before after)
  (declare (hash-table before after))
  (setf (gethash (car pair) after)
		(append (gethash (car pair) after) (list (cdr pair))))
  (setf (gethash (cdr pair) before)
		(append (gethash (cdr pair) before) (list (car pair)))))


(defun clean-all-ele (ele table)
  (loop
	 for k being the hash-keys of table
	 using (hash-value v)
	 when (member ele (gethash k table))
	 do (setf (gethash k table) (remove ele (gethash k table)))))


(defun find-nil-in-table (table)
  (loop
	 for k being the hash-keys of table
	 using (hash-value v)
	 when (not v)
	 collect k))


(defun find-keys-not-in (sets table)
  (let ((keys (loop for k being the hash-keys of table collect k)))
	(member-if-not #'(lambda (x) (member x keys))
				   sets)))


(defun char-to-time (c)
  (- (char-code c) 4))


(defun day7 (filepath)
  (let ((all-input (read-all-input filepath))
		(all-nodes '()) ;actually, it misses endding element
		(char-and-its-after (make-hash-table))
		(char-and-its-before (make-hash-table)))

	;;init hashtable
	(loop
	   for line in all-input
	   ;;do (init-table (parse-input line) char-and-its-before char-and-its-after) ;bug: first element won't in char-and-its-before.
	   do (setf all-nodes (cons (car (parse-input line)) all-nodes))
	   do (add-to-hashtable (parse-input line)
							char-and-its-before
							char-and-its-after))

	;;clean all duplicates
	(setf all-nodes (remove-duplicates all-nodes))
	;;(print all-nodes) (print (length all-nodes))
	;;(print-hashtable char-and-its-before) (print "end")
	;;(print (gethash #\C char-and-its-before))
	;;(print (find-nil-in-table char-and-its-before))
	(do* ((result '())
		  (next-round (delete-duplicates (sort (find-keys-not-in
												all-nodes
												char-and-its-before)
											   #'char<))
					  (delete-duplicates (remove-if #'(lambda (x) (member x result))
													(sort (append next-round
																  (find-nil-in-table char-and-its-before))
														  #'char<)))))
		 ((not next-round) result)
	  (print next-round)
	  (setf result (append result (list (car next-round))))
	  (clean-all-ele (car next-round) char-and-its-before))
	))


(defun re-fill (l l2 num)
  (cond ((= num (length l)) l)
		((< (+ (length l2) (length l)) num)
		 (append l l2))
		(t
		 (subseq (append l l2) 0 5))))


(defun day7-part2 (filepath)
  (let ((all-input (read-all-input filepath))
		(all-nodes '()) ;actually, it misses endding element
		(char-and-its-after (make-hash-table))
		(char-and-its-before (make-hash-table)))

	;;init hashtable
	(loop
	   for line in all-input
	   do (setf all-nodes (cons (car (parse-input line)) all-nodes))
	   do (add-to-hashtable (parse-input line)
							char-and-its-before
							char-and-its-after))

	;;clean all duplicates
	(setf all-nodes (remove-duplicates all-nodes))

	(do* ((time-cost 0)
		  (result '())
		  ;;my input, next-round is not larger than 5, so I write hard code directly
		  (next-round (delete-duplicates (sort (find-keys-not-in
												all-nodes
												char-and-its-before)
											   #'char<))
					  (delete-duplicates (remove-if #'(lambda (x) (or (member x result)
																	  (member x processing)))
													(sort (append next-round
																  (find-nil-in-table char-and-its-before))
														  #'char<))))
		  (processing (subseq next-round 0 5) ;;first 5
					  (re-fill processing next-round 5))
		  (processing-with-time (loop for i in processing collect (cons i (char-to-time i)))
								(if (not next-round)
									processing-with-time
									(append processing-with-time ; input new elements 
											(loop
											   for i in next-round
											   when (member i processing)
											   collect (cons i
															 (char-to-time i)))))))
		 ((not processing) (values result time-cost))
	  ;;(print "next-round: ") (print next-round)
	  ;;(print "processing: ") (print processing)
	  ;;(print "processing time: ") (print processing-with-time)
	  
	  (let ((sort-processing (sort processing-with-time #'< :key #'cdr)))
		;;(print sort-processing)
		(setf time-cost (+ time-cost (cdar sort-processing))
			  result (append result (list (caar sort-processing)))
			  processing (remove (caar sort-processing) processing)
			  processing-with-time (remove (car sort-processing) sort-processing)
			  processing-with-time (loop for i in processing-with-time
									  collect (cons (car i) (- (cdr i) (cdar sort-processing)))))
		(clean-all-ele (caar sort-processing) char-and-its-before)))))
