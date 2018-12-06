(load "./general.lisp")

(defun convert-coop (str-list)
  (mapcar #'read-coordinate str-list))

(defun find-around-points (this near2far short2high)
  (delete-duplicates
   (loop
	  for result in
		(list (find-if (lambda (point) (< (cadr point) (cadr this))) near2far :from-end t)
			  (find-if (lambda (point) (> (cadr point) (cadr this))) near2far)
			  (find-if (lambda (point) (= (cadr point) (cadr this))) near2far)
			  (find-if (lambda (point) (< (car point) (car this))) short2high :from-end t)
			  (find-if (lambda (point) (> (car point) (car this))) short2high)
			  (find-if (lambda (point) (= (car point) (car this))) short2high))
	  when result
	  collect result)))

(defun distance-between-points (this other)
  (apply #'+ (mapcar #'(lambda (x y) (abs (- y x))) this other)))

(defun distance-between-this-to-all-points (this others)
  (apply #'+ (mapcar #'(lambda (x)
						 (+ (abs (- (car this) (car x)))
							(abs (- (cadr this) (cadr x)))))
					 others)))

(defun which-is-closest (this others)
  (let* ((sorted-results (sort (mapcar #'(lambda (other)
										   (cons other (distance-between-points this other)))
									   others)
							   #'(lambda (a b) (< (cdr a) (cdr b)))))
		 (min-value (cdar sorted-results)))
	;;(print sorted-results)
	(loop
	   for ele in sorted-results
	   when (eql (cdr ele) min-value)
	   collect ele)))

(defun day6 (filepath)
  (let* ((all-points (convert-coop (read-all-input filepath)))
		 (near2far (sort (copy-seq all-points) #'< :key #'cadr))
		 (nearest (cadar near2far))
		 (farest (cadar (last near2far)))
		 (short2high (sort (copy-seq all-points) #'< :key #'car))
		 (shortest (caar short2high))
		 (highest (caar (last short2high)))
		 (points (make-hash-table :test 'equal)))

	(loop
	   for point in all-points
	   do (setf (gethash point points) 0))
	;;(pprint points)
	(print near2far) (print short2high)
	;;(print nearest) (print farest) (print shortest) (print highest)
	;;(find-around-points '(175 152) near2far short2high)

	(loop
	   for i from (1+ nearest) to (1- farest)
	   do (loop
			 for ii from (1+ shortest) to (1- highest)
			 do (let* (;(around-points (find-around-points (list ii i) near2far short2high))
					   ;;actually, find-around-points is bad idea, use all points instead
					   (around-points all-points)
					   (results (which-is-closest (list ii i) around-points)))
				  ;;(print i) (print ii) (print (find-around-points (list ii i) near2far short2high))
				  (if (= 1 (length results))
					  (setf (gethash (caar results) points)
							(incf (gethash (caar results) points)))))))

	(sort (loop
			 for k being the hash-keys of points
			 using (hash-value v)
			 collect(cons k v))
		  #'(lambda (x y) (< (cdr x) (cdr y))))
	))

(defun day6-part2 (filepath)
  (let* ((all-points (convert-coop (read-all-input filepath)))
		 (gap (multiple-value-bind (x y)
				  (round 10000 (length all-points))
				x))
		 (near2far (sort (copy-seq all-points) #'< :key #'cadr))
		 (nearest (- (cadar near2far) gap))
		 (farest (+ (cadar (last near2far)) gap))
		 (short2high (sort (copy-seq all-points) #'< :key #'car))
		 (shortest (- (caar short2high) gap))
		 (highest (+ (caar (last short2high)) gap))
		 (result 0))

	(loop
	   for i from (1+ nearest) to (1- farest)
	   do (loop
			 for ii from (1+ shortest) to (1- highest)
			 do (if (< (distance-between-this-to-all-points (list ii i) all-points) 10000)
					(incf result))))
	result
	))
