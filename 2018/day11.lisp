(defun rack-id (po)
  "po = (a . b)"
  (+ 10 (car po)))


(defun power-level (po)
  (* (cdr po)
	 (rack-id po)))


(defun add-serial (po serial)
  (+ serial (power-level po)))


(defun mutil-rack (po serial)
  (* (add-serial po serial)
	 (rack-id po)))


(defun sub-5 (po serial)
  (let ((mutilrack (mutil-rack po serial)))
	(- (mod (/ (- mutilrack (mod mutilrack 100)) 100) 10) 5)))


(defun day11 (serial)
  (let ((all-points (make-array '(300 300)))
		(result-list '()))
	(loop
	   for i from 0 to 299
	   do (loop
			 for ii from 0 to 299
			 do (setf (aref all-points i ii)
					  (sub-5 (cons i ii) serial))))

	(setf result-list
		  (loop
			 with re = '()
			 for i from 0 to 297
			 do (setf re
					  (append re
							  (loop
								 for ii from 0 to 297
								 collect (cons (list i ii 3)
											   (loop
												  for i1 from 0 to 2
												  sum 
													(loop
													   for ii1 from 0 to 2
													   sum (aref all-points (+ i1 i) (+ ii1 ii))))))))
			 finally (return re)))
	;;all-points
	result-list
	))

(let ((a (day11 2694))) (last (sort a #'< :key #'cdr)))

(defun day11-part2 (serial)
  (let ((all-points (make-array '(300 300)))
		(result-list '()))
	(loop
	   for i from 0 to 299
	   do (loop
			 for ii from 0 to 299
			 do (setf (aref all-points i ii)
					  (sub-5 (cons i ii) serial))))

	(do ((size 1 (1+ size)))
		((= size 300))
	  (setf result-list
			(append result-list (loop
								   with re = '()
								   for i from 0 to 297
								   do (setf re
											(append re
													(loop
													   for ii from 0 to 297
													   collect (cons (list i ii size)
																	 (loop
																		for i1 from 0 to 2
																		sum 
																		  (loop
																			 for ii1 from 0 to 2
																			 sum (aref all-points (+ i1 i) (+ ii1 ii))))))))
								   finally (return re)))))
	;;all-points
	result-list
	))

(let ((a (day11-part2 2694))) (last (sort a #'< :key #'cdr)))
