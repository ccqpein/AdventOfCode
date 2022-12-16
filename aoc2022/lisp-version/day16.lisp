(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day16.input"))
(defparameter *demo-input* (read-file-by-line "../inputs/day16_demo.input"))

(defun str-trim-right-char (s c)
  (let ((ss (mapcar #'string (concatenate 'list s))))
	(loop for cc on (reverse ss)
		  when (string/= c (car cc))
			return (str:join "" (reverse cc))
		  )))

(defun parse-line (line)
  (let ((ss (str:split " " line)))
	(list (second ss) ;; name
		  (str-trim-right-char (car (last (str:split "=" (nth 4 ss)))) ";")
		  (or (mapcar (lambda (x) (str-trim-right-char x ",")) (cdr (member "valves" ss :test 'string=)))
			  (mapcar (lambda (x) (str-trim-right-char x ",")) (cdr (member "valve" ss :test 'string=)))))
	))

(defun gen-graph (&optional (input *input*))
  (loop with g = (make-hash-table :test 'equal)
		for line in input
		for (name v to) = (parse-line line)
		do (setf (gethash name g) (list (parse-integer v) to))
		finally (return g)
		))

(defun part1 (graph)
  (loop for (v next) = (gethash "AA" graph)
		return (loop for n in next
					 maximize (move n graph 29
									(make-hash-table :test 'equal)
									'()
									(all-valves-has-rate graph)))
		))

(defun all-valves-has-rate (graph)
  (loop
	with set = (make-hash-set)
	for k being the hash-key using (hash-value (v next)) of graph
	when (/= 0 v)
	  do (set-insert set k)
	finally (return set)
	))

(defun move (start graph min-left cache already has-rate)
  (format t "start: ~a, min-left: ~a, already: ~a~%" start min-left already)
  (if (<= min-left 0) (return-from move 0))
  
  (if (gethash (list start min-left (sort already 'string<)) cache)
	  (return-from move (gethash (list start min-left already) cache)))

  (if (every (lambda (v) (member v already :test 'string=)) (set-to-list has-rate))
	  (return-from move 0))
  
  (let* ((value (gethash start graph))
		 (v (first value))
		 (next (second value))
		 result)
	
	(setf result
		  (if (= 0 v)
			  (loop for n in next
					maximize (move n graph (1- min-left) cache already has-rate))
			  ;; two other options
			  (max (loop for n in next
						 maximize (move n graph (1- min-left) cache already has-rate)
						 ;;do (if (string= "EE" n) (format t "what a fuck: ~a, ~a~%" start n))
						 )
				   
				   (if (not (member start already :test 'string=))
					   (progn
						 (format t "start: ~a, already: ~a~%" start already)
						 (loop
						   for n in next
						   maximize (+ (* v (1- min-left))
									   (progn
										 (format t "in loop: start:~a, already: ~a, check: ~a~%"
												 start already (not (member start already :test 'string=)))
										 (move n graph (- min-left 2) cache (cons start already) has-rate)))
						   ;; do (if (string= "EE" n)
						   ;; 		  (format t "what a fuck in loop: ~a ~a ~a~%" start n already))
						   ))
					   0))))
	(if (equal already '("EE" "EE")) (format t "what the fuck: ~a~%" start))
	(setf (gethash (list start min-left (sort already 'string<)) cache) result)
	result
	))

(let ((cache (make-hash-table :test 'equal)))
  (move "DD" (gen-graph *demo-input*) 6 cache '() (all-valves-has-rate (gen-graph *demo-input*)))
  (loop for k being the hash-key using (hash-value v) of cache
		do (format t "key: ~a, value: ~a~%" k v)))

;; (if (not (member "EE" (sort '("EE") 'string<) :test 'string=))
;; 	(progn (print t)
;; 		   (loop for i from 0 to 10
;; 				 maximize (progn (print "in loop")
;; 								 (not (member "EE" (sort '("EE") 'string<) :test 'string=))
;; 								 0)))
;; 	0)
