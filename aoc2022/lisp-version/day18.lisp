(load "../../tools/tools.lisp")

(defparameter *demo-input* (read-file-by-line "../inputs/day18_demo.input"))
(defparameter *input* (read-file-by-line "../inputs/day18.input"))

(defstruct cube
  x y z open-surface)

(defun 1-surface (cube)
  (decf (cube-open-surface cube)))

(defun side-with (a b)
  (when (side-with-p a b)
	(1-surface a) (1-surface b)))

(defun side-with-p (a b)
  (cond ((and (= (cube-x a) (cube-x b)) (= (cube-y a) (cube-y b)))
		 (= 1 (abs (- (cube-z a) (cube-z b)))))
		((and (= (cube-x a) (cube-x b)) (= (cube-z a) (cube-z b)))
		 (= 1 (abs (- (cube-y a) (cube-y b)))))
		((and (= (cube-z a) (cube-z b)) (= (cube-y a) (cube-y b)))
		 (= 1 (abs (- (cube-x a) (cube-x b)))))))

(defun parse-line (line)
  (let ((ss (mapcar #'parse-integer (str:split "," line))))
	(make-cube :x (first ss) :y (second ss) :z (third ss)
			   :open-surface 6)))

(defun insert-hash-table (table v &rest keys)
  (if (= 1 (length keys))
	  (if (gethash (car keys) table)
		  (setf (gethash (car keys) table) v)
		  (progn (setf (gethash (car keys) table) (make-hash-table :test 'equal))
				 (setf (gethash (car keys) table) v)))
	  (if (gethash (car keys) table)
		  (apply #'insert-hash-table (gethash (car keys) table) v (cdr keys))
		  (progn (setf (gethash (car keys) table) (make-hash-table :test 'equal))
				 (apply #' insert-hash-table (gethash (car keys) table) v (cdr keys))))))

(defun make-table (&optional (input *input*))
  (let ((x-y-table (make-hash-table :test 'equal))
		(x-z-table (make-hash-table :test 'equal))
		(y-z-table (make-hash-table :test 'equal)))
	(loop for line in input
		  for c = (parse-line line)
		  do (insert-hash-table x-y-table c (cube-x c) (cube-y c) (cube-z c))
		  do (insert-hash-table x-z-table c (cube-x c) (cube-z c) (cube-y c))
		  do (insert-hash-table y-z-table c (cube-y c) (cube-z c) (cube-x c))
		  collect c into all-cubes
		  finally (return (values all-cubes x-y-table x-z-table y-z-table)))))

(defun table-p (table a b)
  (and (gethash a table) (gethash b (gethash a table))))

(defun table-p-3 (table a b c)
  (and (gethash a table)
	   (gethash b (gethash a table))
	   (gethash c (gethash b (gethash a table)))))

(defun part1 (&optional (input *input*))
  (let (all x-y-table x-z-table y-z-table
		x-min x-max y-min y-max z-min z-max)
	
	(multiple-value-setq (all x-y-table x-z-table y-z-table)
	  (make-table input))
	
	(loop for line in input
		  for (x y z) = (mapcar #'parse-integer (str:split "," line))
		  collect x into xx
		  collect y into yy
		  collect z into zz
		  finally (setf x-min (apply #'min xx) x-max (apply #'max xx)
						y-min (apply #'min yy) y-max (apply #'max yy)
						z-min (apply #'min zz) z-max (apply #'max zz)))

	(loop for x from x-min to x-max
		  do (loop for y from y-min to y-max
				   if (table-p x-y-table x y)
					 do (loop with all-c = (alexandria:hash-table-values (gethash y (gethash x x-y-table)))
							  for i from 0 below (1- (length all-c))
							  do (loop for j from (1+ i) below (length all-c)
									   do (side-with (nth i all-c) (nth j all-c))))))

	(loop for x from x-min to x-max
		  do (loop for z from z-min to z-max
				   if (table-p x-z-table x z)
					 do (loop with all-c = (alexandria:hash-table-values (gethash z (gethash x x-z-table)))
							  for i from 0 below (1- (length all-c))
							  do (loop for j from (1+ i) below (length all-c)
									   do (side-with (nth i all-c) (nth j all-c))))))

	(loop for y from y-min to y-max
		  do (loop for z from z-min to z-max
				   if (table-p y-z-table y z)
					 do (loop with all-c = (alexandria:hash-table-values (gethash z (gethash y y-z-table)))
							  for i from 0 below (1- (length all-c))
							  do (loop for j from (1+ i) below (length all-c)
									   do (side-with (nth i all-c) (nth j all-c))))))
	
	;;(format t "~a~%" (alexandria:hash-table-values (gethash 2 (gethash 2 y-z-table))))
	;;(format t "~a~%" all)
	(apply #'+ (mapcar (lambda (c) (cube-open-surface c)) all))))

(defun part2 (&optional (input *input*))
  (let (all x-y-table x-z-table y-z-table
		x-min x-max y-min y-max z-min z-max
		all-void-cubes
		)
	
	(multiple-value-setq (all x-y-table x-z-table y-z-table)
	  (make-table input))
	
	(loop for line in input
		  for (x y z) = (mapcar #'parse-integer (str:split "," line))
		  collect x into xx
		  collect y into yy
		  collect z into zz
		  finally (setf x-min (apply #'min xx) x-max (apply #'max xx)
						y-min (apply #'min yy) y-max (apply #'max yy)
						z-min (apply #'min zz) z-max (apply #'max zz)))

	;; full all cubes
	(setf all-void-cubes
		  (loop for x from x-min to x-max
				append (loop for y from y-min to y-max
							 when (table-p x-y-table x y)
							   append (loop
										with z-here =
													(sort (alexandria:hash-table-keys
														   (gethash y (gethash x x-y-table)))
														  #'<)
										for z from (first z-here) to (car (last z-here))
										for cc = (make-cube :x x :y y :z z :open-surface 6)
										if (not (table-p-3 x-y-table x y z))
										  do (progn
											   (insert-hash-table x-y-table cc x y z)
											   (insert-hash-table y-z-table cc y z x)
											   (insert-hash-table x-z-table cc x z y))
										  and collect cc))))

	(loop for x from x-min to x-max
		  do (loop for y from y-min to y-max
				   if (table-p x-y-table x y)
					 do (loop with all-c = (alexandria:hash-table-values (gethash y (gethash x x-y-table)))
							  for i from 0 below (1- (length all-c))
							  do (loop for j from (1+ i) below (length all-c)
									   do (side-with (nth i all-c) (nth j all-c))))))

	(loop for x from x-min to x-max
		  do (loop for z from z-min to z-max
				   if (table-p x-z-table x z)
					 do (loop with all-c = (alexandria:hash-table-values (gethash z (gethash x x-z-table)))
							  for i from 0 below (1- (length all-c))
							  do (loop for j from (1+ i) below (length all-c)
									   do (side-with (nth i all-c) (nth j all-c))))))

	(loop for y from y-min to y-max
		  do (loop for z from z-min to z-max
				   if (table-p y-z-table y z)
					 do (loop with all-c = (alexandria:hash-table-values (gethash z (gethash y y-z-table)))
							  for i from 0 below (1- (length all-c))
							  do (loop for j from (1+ i) below (length all-c)
									   do (side-with (nth i all-c) (nth j all-c))))))

	;;(print (length (loop for c in all-void-cubes when (= 0 (cube-open-surface c)) collect c)))
	
	(multiple-value-setq (all x-y-table x-z-table y-z-table)
	  (make-table input))
	
	(- (part1 input)
	   (loop
		 with checked-cubes = (loop for c in all-void-cubes when (= 0 (cube-open-surface c)) collect c)
		 for x from x-min to x-max
		 sum (loop for y from y-min to y-max
				   sum (loop for z from z-min to z-max
							 when (table-p-3 x-y-table x y z)
							   sum (loop for c in checked-cubes
										 when (side-with-p (gethash z (gethash y (gethash x x-y-table)))
														   c)
										   count 1)))))
	))


(multiple-value-bind (all x-y-table x-z-table y-z-table)
	(make-table *demo-input*)
  (1-surface (gethash 2 (gethash 2 (gethash 2 x-y-table))))  
  (print (gethash 2 (gethash 2 (gethash 2 x-y-table))))
  (print (gethash 2 (gethash 2 (gethash 2 x-z-table))))
  (print all)
  )

;16816
;4042
;264
;2453
