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
					 maximize (move n graph 30 (make-hash-table :test 'equal)))
		))

(defun move (start graph min-left cache)
  ;;(format t "start: ~a, min-left: ~a~%" start min-left)
  (if (< min-left 0) (return-from move 0))
  (if (gethash (list start min-left) cache)
	  (gethash (list start min-left) cache))
  
  (let* ((value (gethash start graph))
		 (v (first value))
		 (next (second value))
		 result)
	(setf result
		  (if (= 0 v)
			  (loop for n in next
					maximize (move n graph (1- min-left) cache))
			  ;; two other options
			  (max (loop for n in next
						 maximize (move n graph (1- min-left) cache))
				   (loop for n in next
						 maximize (+ (* v (1- min-left)) (move n graph (- min-left 2) cache))))))
	(setf (gethash (list start min-left) cache) result)
	result
	))
