(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day14.input"))
(defparameter *demo-input* (read-file-by-line "../inputs/day14_demo.input"))

(defun make-map (&optional (input *input*))
  (loop
	with map = (make-hash-table :test 'equal)
	for line in input
	for nodes = (str:split " -> " line)
	do (reduce (lambda (a b)
				 (draw-line map
							(mapcar #'parse-integer (str:split "," a))
							(mapcar #'parse-integer (str:split "," b)))
				 b)
			   nodes)
	finally (return map)
	))

(defun draw-line (map a b)
  (if (= (first a) (first b))
	  (loop for i from (min (second a) (second b)) to (max (second a) (second b))
			do (setf (gethash (list (first a) i) map) "#"))
	  (loop for i from (min (first a) (first b)) to (max (first a) (first b))
			do (setf (gethash (list i (second a)) map) "#"))))

(defun one-step (map sand-x sand-y)
  (let ((n (gethash (list sand-x (1+ sand-y)) map)))
	(cond
	  ((not n) (list sand-x (1+ sand-y)))
	  ((or (string= n "O") (string= n "#"))
	   (cond
		 ((not (gethash (list (1- sand-x) (1+ sand-y)) map)) (list (1- sand-x) (1+ sand-y)))
		 ((not (gethash (list (1+ sand-x) (1+ sand-y)) map)) (list (1+ sand-x) (1+ sand-y)))
		 (t (list sand-x sand-y)))))))

(defun skyfall (map part &optional how-many)
  (let ((map-edge (loop for k being each hash-key of map
						maximize (second k))))
	(loop
	  with count = 0
	  for (pos-x pos-y) = '(500 0)
	  if (and how-many (= count how-many))
		return count
	  else
		do (loop for (new-pos-x new-pos-y) = (one-step map pos-x pos-y)
				 when (end-option new-pos-x new-pos-y map-edge part)
				   do (return-from skyfall count)
				 when (legal-sand-pos pos-x pos-y new-pos-x new-pos-y map-edge part)
				   do (setf (gethash (list new-pos-x new-pos-y) map) "O")
				   and do (incf count)
				   and do (return)
				 do (setf pos-x new-pos-x
						  pos-y new-pos-y)
				 ))))

(defun end-option (x y map-edge part)
  (if (= 1 part)
	  (>= y map-edge)
	  (and (= x 500) (= y 0))))

(defun legal-sand-pos (x y new-x new-y map-edge part)
  (if (= 1 part)
	  (and (= x new-x) (= y new-y))
	  (or (and (= x new-x) (= y new-y)) (= new-y (1+ map-edge)))))

(defun day14 (part &optional (input *input*))
  (if (= 1 part)
	  (skyfall (make-map input) part)
	  (1+ (skyfall (make-map input) part))))
