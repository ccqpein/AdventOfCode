(load "../../tools/tools.lisp")

(defparameter *monkeys*
  (list 
   (list 0 '(91 66)
		 (lambda (old) (* 13 old))
		 (lambda (items) (= 0 (mod (/ items 19) 1)))
		 6 2)
   (list 1 '(78 97 59)
		 (lambda (old) (+ 7 old))
		 (lambda (items) (= 0 (mod (/ items 5) 1)))
		 0 3)
   (list 2 '(57 59 97 84 72 83 56 76)
		 (lambda (old) (+ 6 old))
		 (lambda (items) (= 0 (mod (/ items 11) 1)))
		 5 7)
   (list 3 '(81 78 70 58 84)
		 (lambda (old) (+ 5 old))
		 (lambda (items) (= 0 (mod (/ items 17) 1)))
		 6 0)
   (list 4 '(60)
		 (lambda (old) (+ 8 old))
		 (lambda (items) (= 0 (mod (/ items 7) 1)))
		 1 3)
   (list 5 '(57 69 63 75 62 77 72)
		 (lambda (old) (* 5 old))
		 (lambda (items) (= 0 (mod (/ items 13) 1)))
		 7 4)
   (list 6 '(73 66 86 79 98 87)
		 (lambda (old) (* old old))
		 (lambda (items) (= 0 (mod (/ items 3) 1)))
		 5 2)
   (list 7 '(95 89 63 67)
		 (lambda (old) (+ 2 old))
		 (lambda (items) (= 0 (mod (/ items 2) 1)))
		 1 4))
  )

(defparameter *monkeys-divider* (* 19 5 11 17 7 13 3 2))

(defparameter *demo-monkeys*
  (list 
   (list 0 ;; monkey num
		 '(79 98) ;; start items
		 (lambda (old) (* 19 old)) ;; formula
		 (lambda (items) (= 0 (mod (/ items 23) 1))) ;; check
		 2 3) ;; target
   (list 1 '(54 65 75 74) (lambda (old) (+ 6 old)) (lambda (items) (= 0 (mod (/ items 19) 1))) 2 0)
   (list 2 '(79 60 97) (lambda (old) (* old old)) (lambda (items) (= 0 (mod (/ items 13) 1))) 1 3)
   (list 3 '(74) (lambda (old) (+ 3 old)) (lambda (items) (= 0 (mod (/ items 17) 1))) 0 1))
  )

(defparameter *demo-monkeys-divider* (* 23 19 13 17))

(defun day11 (&optional part2)
  (let (;;(*monkeys* *demo-monkeys*)
		;;(*monkeys-divider* *demo-monkeys-divider*)
		)
	(if (not part2)
		(apply #'* (subseq (sort (mutle-round *monkeys* 20) #'>) 0 2))
		(apply #'* (subseq (sort (mutle-round *monkeys* 10000 part2) #'>) 0 2))))
  )

(defun mutle-round (monkeys times &optional part2)
  (loop
	with result = (make-list (length monkeys) :initial-element 0)
	for i from 1 to times
	for x = (one-round monkeys part2)
	do (loop for j from 0 to (length result)
			 if (nth j x)
			   do (incf (nth j result) (nth j x))
			 end)
	finally (return result)
	))

(defun one-round (monkeys &optional part2)
  (loop for monkey in monkeys
		collect (length (nth 1 monkey)) into x
		do (one-monkey-move monkey monkeys part2)		   
		finally (return x)))

(defun one-monkey-move (this monkeys &optional part2)
  (loop for it in (nth 1 this)
		for items = (funcall (nth 2 this) it)
		for d = (if part2 items (floor (/ items 3)))
		if (funcall (nth 3 this) d)
		  do (let ((before (nth 1 (nth (nth 4 this) monkeys))))
			   (setf (nth 1 (nth (nth 4 this) monkeys))
					 (append before (list (mod d *monkeys-divider*)))))
		else
		  do (let ((before (nth 1 (nth (nth 5 this) monkeys))))
			   (setf (nth 1 (nth (nth 5 this) monkeys))
					 (append before (list (mod d *monkeys-divider*)))))
		finally (setf (nth 1 this) nil)))

