(load "./general.lisp")


(defun day5 (filepath)
  (let ((cache (read-all-input-together filepath))
		(result '()))
	(do* ((need-to-delete '(1))
		  (this-line cache (clean-list this-line need-to-delete))
		  )
		((eql nil need-to-delete) this-line)
	  ;;(print this-line)
	  (setf need-to-delete '())
	  
	  (do ((ind 0))
		  ((>= ind (- (length this-line) 1)))
		(if (case-match (nth ind this-line) (nth (1+ ind) this-line))
			(setf need-to-delete (append need-to-delete (list ind (1+ ind)))
				  ind (+ 2 ind))
			(incf ind))
		))))


(defun stack-handle (char-list)
  "second version of day5, this method make thing go in O(n)"
  (let ((len (length char-list)))
	(do ((this-char 1 (incf this-char))
		 (stack (list (car char-list))))
		((eql this-char len) stack)
	  (cond ((eql nil stack)
			 (setf stack (cons (nth this-char char-list) stack)))
			((case-match (nth this-char char-list)
						 (car stack))
			 (setf stack (cdr stack)))
			(t
			 (setf stack (cons (nth this-char char-list) stack)))))))


(defun day5-v2 (filepath)
  "use new stack-handle"
  (let ((cache (read-all-input-together filepath)))
	(length (stack-handle cache))))


(defun day5-part2 (filepath)
  (let ((cache (read-all-input-together filepath))
		(alphabet (mapcar #'list
						  (concatenate 'list "abcdefghigklmnopqrstuvwxyz")
						  (concatenate 'list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))))
	(loop
	   for i from 0 to 25
	   for ab = (nth i alphabet)
	   collect (let ((inner-cache (remove-from-list cache ab))
					 )
				(do* ((need-to-delete '(1))
					  (this-line inner-cache (clean-list this-line need-to-delete))
					  )
					 ((eql nil need-to-delete) (cons ab (length this-line)))
				  ;;(print this-line)
				  (setf need-to-delete '())
				  
				  (do ((ind 0))
					  ((>= ind (- (length this-line) 1)))
					(if (case-match (nth ind this-line) (nth (1+ ind) this-line))
						(setf need-to-delete (append need-to-delete (list ind (1+ ind)))
							  ind (+ 2 ind))
						(incf ind))
					))))))


(defun day5-part2-v2 (filepath)
  (let ((cache (read-all-input-together filepath))
		(alphabet (mapcar #'list
						  (concatenate 'list "abcdefghigklmnopqrstuvwxyz")
						  (concatenate 'list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))))
	(loop
	   for i from 0 to 25
	   for ab = (nth i alphabet)
	   collect (let ((inner-cache (remove-from-list cache ab)))
				 (length (stack-handle inner-cache))))))


(defun case-match (a b)
  (cond ((eql (upper-case-p a) (upper-case-p b))
		 nil)
		(t
		 (eql (char-upcase a) (char-upcase b)))))


(defun clean-list (l need-delete)
  (loop for i from 0 to (- (length l) 1)
		when (not (member i need-delete))
	 collect (nth i l)))


(defun remove-from-list (l char-pair)
  (remove (nth 0 char-pair)
		  (remove (nth 1 char-pair) l)))
