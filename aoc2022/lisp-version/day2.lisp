(load "../../tools/tools.lisp")

(defun cal-point-1 (op me)
  (cond ((string= "A" op)
		 (cond ((string= "Y" me) (+ 2 6))
			   ((string= "X" me) (+ 1 3))
			   ((string= "Z" me) (+ 3 0))))
		
		((string= "B" op)
		 (cond ((string= "Y" me) (+ 2 3))
			   ((string= "X" me) (+ 1 0))
			   ((string= "Z" me) (+ 3 6))))
		
		((string= "C" op)
		 (cond ((string= "Y" me) (+ 2 0))
			   ((string= "X" me) (+ 1 6))
			   ((string= "Z" me) (+ 3 3))))))

;; (defun day2 ()
;;   (let ((input (read-file-by-line "../inputs/day2.input"))
;; 		)
;; 	(loop for line in input
;; 		  for (op me) = (str:split " " line)
;; 		  sum (cal-point-1 op me))))

(defun cal-point-2 (op me)
  (cond ((string= "A" op)
		 (cond ((string= "Y" me) (+ 1 3))
			   ((string= "X" me) (+ 3 0))
			   ((string= "Z" me) (+ 2 6))))
		
		((string= "B" op)
		 (cond ((string= "Y" me) (+ 2 3))
			   ((string= "X" me) (+ 1 0))
			   ((string= "Z" me) (+ 3 6))))
		
		((string= "C" op)
		 (cond ((string= "Y" me) (+ 3 3))
			   ((string= "X" me) (+ 2 0))
			   ((string= "Z" me) (+ 1 6))))))

;; (defun day2-part2 ()
;;   (let ((input (read-file-by-line "../inputs/day2.input"))
;; 		)
;; 	(loop for line in input
;; 		  for (op me) = (str:split " " line)
;; 		  sum (cal-point-2 op me))))

(defun day2 (part)
  (let ((input (read-file-by-line "../inputs/day2.input"))
		(cal-fn (ccase part
				  (1 #'cal-point-1)
				  (2 #'cal-point-2))))
	(loop for line in input
		  for (op me) = (str:split " " line)
		  sum (funcall cal-fn op me))
	))
