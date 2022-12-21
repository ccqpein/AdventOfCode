(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day21.input"))
(defparameter *demo-input* (read-file-by-line "../inputs/day21_demo.input"))

;; (defun parse-input (&optional (input *input*))
;;   (loop with table = (make-hash-table :test 'equal)
;; 		for line in input
;; 		for ss = (str:split " " line)
;; 		for name = (car (str:split ":" (car ss)))
;; 		if (> (length (cdr ss)) 1)
;; 		  do (setf (gethash name table)
;; 				   (cond ((string= (second (cdr ss)) "+")
;; 						  (let ((a (first (cdr ss)))
;; 								(b (third (cdr ss))))
;; 							(lambda (table)
;; 							  (if (and (numberp (gethash a table))
;; 									   (numberp (gethash b table)))
;; 								  (+ (gethash a table)
;; 									 (gethash b table))))))
;; 						 ((string= (second (cdr ss)) "-")
;; 						  (let ((a (first (cdr ss)))
;; 								(b (third (cdr ss))))
;; 							(lambda (table)
;; 							  (if (and (numberp (gethash a table))
;; 									   (numberp (gethash b table)))
;; 								  (- (gethash a table)
;; 									 (gethash b table))))))
;; 						 ((string= (second (cdr ss)) "*")
;; 						  (let ((a (first (cdr ss)))
;; 								(b (third (cdr ss))))
;; 							(lambda (table)
;; 							  (if (and (numberp (gethash a table))
;; 									   (numberp (gethash b table)))
;; 								  (* (gethash a table)
;; 									 (gethash b table))))))
;; 						 ((string= (second (cdr ss)) "/")
;; 						  (let ((a (first (cdr ss)))
;; 								(b (third (cdr ss))))
;; 							(lambda (table)
;; 							  (if (and (numberp (gethash a table))
;; 									   (numberp (gethash b table)))
;; 								  (/ (gethash a table)
;; 									 (gethash b table))))))
;; 						 ))
;; 		else
;; 		  do (setf (gethash name table) (parse-integer (cadr ss)))
;; 		finally (return table)
;; 		))



;; (defun part1 (&optional (input *input*))
;;   (let ((table (parse-input input)))
;; 	(loop
;; 	  with flag = t
;; 	  if flag
;; 		do (setf flag nil) 
;; 		and do (loop
;; 				 for k being the hash-key
;; 				   using (hash-value v) of table
;; 				 if (and (functionp v) (funcall v table))
;; 				   do (setf (gethash k table) (funcall v table)
;; 							flag t))
;; 	  else
;; 		return nil)
;; 	(gethash "root" table)))

;; (defun parse-input-2 (&optional (input *input*))
;;   (loop with table = (make-hash-table :test 'equal)
;; 		and first and second
		
;; 		for line in input
;; 		for ss = (str:split " " line)
;; 		for name = (car (str:split ":" (car ss)))
;; 		if (string= name "root")
;; 		  do (setf first (first (cdr ss))
;; 				   second (third (cdr ss)))
;; 		else 
;; 		  do (if (> (length (cdr ss)) 1)
;; 				 (setf (gethash name table)
;; 					   (cond ((string= (second (cdr ss)) "+")
;; 							  (let ((a (first (cdr ss)))
;; 									(b (third (cdr ss))))
;; 								(lambda (table)
;; 								  (if (and (numberp (gethash a table))
;; 										   (numberp (gethash b table)))
;; 									  (+ (gethash a table)
;; 										 (gethash b table))))))
;; 							 ((string= (second (cdr ss)) "-")
;; 							  (let ((a (first (cdr ss)))
;; 									(b (third (cdr ss))))
;; 								(lambda (table)
;; 								  (if (and (numberp (gethash a table))
;; 										   (numberp (gethash b table)))
;; 									  (- (gethash a table)
;; 										 (gethash b table))))))
;; 							 ((string= (second (cdr ss)) "*")
;; 							  (let ((a (first (cdr ss)))
;; 									(b (third (cdr ss))))
;; 								(lambda (table)
;; 								  (if (and (numberp (gethash a table))
;; 										   (numberp (gethash b table)))
;; 									  (* (gethash a table)
;; 										 (gethash b table))))))
;; 							 ((string= (second (cdr ss)) "/")
;; 							  (let ((a (first (cdr ss)))
;; 									(b (third (cdr ss))))
;; 								(lambda (table)
;; 								  (if (and (numberp (gethash a table))
;; 										   (numberp (gethash b table)))
;; 									  (/ (gethash a table)
;; 										 (gethash b table))))))
;; 							 ))
;; 				 (setf (gethash name table) (parse-integer (cadr ss))))
;; 		finally (return (values first second table))
;; 		))

;; (defun part2 (&optional (input *input*))
;;   (let (table first second)
	
;; 	(multiple-value-setq (first second table)
;; 	  (parse-input-2 input))
	
;; 	(loop for h from 0
;; 		  for new-table = (alexandria:copy-hash-table table)
;; 		  do (setf (gethash "humn" new-table) h)
;; 		  if (loop
;; 			   with flag = t
;; 			   if flag
;; 				 do (setf flag nil) 
;; 				 and do (loop
;; 						  for k being the hash-key
;; 							using (hash-value v) of new-table
;; 						  if (and (functionp v) (funcall v new-table))
;; 							do (setf (gethash k new-table) (funcall v new-table)
;; 									 flag t))
;; 			   else
;; 				 return (= (gethash first new-table) (gethash second new-table)))
;; 			return h
;; 		  )
;; 	;;(gethash "root" table)
;; 	))


(defun parse-input-3 (&optional (input *input*))
  (loop with table = (make-hash-table :test 'equal)
		for line in input
		for ss = (str:split " " line)
		for name = (car (str:split ":" (car ss)))
		for value = (if (> (length (cdr ss)) 1)
						(list (third ss) (second ss) (fourth ss))
						(cadr ss))
		do (setf (gethash name table) value)
		finally (return table)))

(defun to-expression (table key cache)
  (declare (special *part*))
  (if (gethash key cache)
	  (return-from to-expression (gethash key cache)))
  
  (if (string= key "humn")
	  (return-from to-expression (read-from-string "humn")))
  
  (if (handler-case (parse-integer key) (error () nil))
	  (return-from to-expression (parse-integer key)))
  
  (let ((vv (if (consp (gethash key table))
				(list (if (string= "root" key)
						  (if (= 1 *part*)
							  (read-from-string (first (gethash key table))) ;; part1 
							  (read-from-string "=")) ;; part2
						  (read-from-string (first (gethash key table))))
					  
					  (handler-case (eval (to-expression table (second (gethash key table)) cache))
						(error () (to-expression table (second (gethash key table)) cache)))
					  
					  (handler-case (eval (to-expression table (third (gethash key table)) cache))
						(error () (to-expression table (third (gethash key table)) cache))))
				
				(parse-integer (gethash key table)))))
	(setf (gethash key cache) vv)
	vv
	))

(defun reverse-back (expression result)
  (let (value exp
		value-ind expression-ind
		new-result)

	(typecase (second expression)
	  (integer (setf value (second expression)
					 exp (third expression)
					 value-ind 1
					 expression-ind 2))
	  (t (setf value (third expression)
			   exp (second expression)
			   value-ind 2
			   expression-ind 1)))
	
	(setf new-result
		  (case (first expression)
			(+ (- result value))
			(- (if (= 1 value-ind)
				   (- value result)
				   (+ value result)))
			(* (/ result value))
			(/ (if (= 1 value-ind)
				   (/ value result)
				   (* value result)))
			))
	
	(if (equal 'HUMN exp)
		new-result
		(reverse-back exp new-result)
		)))

(defun part2 (&optional (input *input*))
  (let* ((table (parse-input-3 input))
		 (*part* 2)
		 form)
	(declare (special *part*))
	(setf form (to-expression table "root" (make-hash-table :test 'equal)))
	(typecase (second form)
	  (cons (reverse-back (second form) (third form)))
	  (t (reverse-back (third form) (second form))))
	))

(defun part1 (&optional (input *input*))
  (let ((table (parse-input-3 input))
		(HUMN 2326)
		(*part* 1))
	(declare (special HUMN *part*))
	(eval (to-expression table "root" (make-hash-table :test 'equal)))
	))
