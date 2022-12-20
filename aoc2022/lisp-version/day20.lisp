(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day20.input"))
(defparameter *demo-input* (read-file-by-line "../inputs/day20_demo.input"))

(defun parse-input (&optional (input *input*))
  (loop for line in input
		for n = (parse-integer line)
		collect n))

(defun circle-it (ll)
  (setf (cdr (last ll)) ll))

(defun move (n ll offset len)
  (loop if (= (second ll) n)
		  return nil
		else 
		  do (setf ll (cdr ll)))

  (format t "find ~a~%" (print-circle ll 10))
  (setf (cdr ll) (nthcdr 2 ll)
		ll (cdr ll))
  
  (format t "delete it ~a~%" (print-circle ll 7))
  
  (cond ((< offset 0)
		 (setf ll (nthcdr (1- (neg-offset-adjust len offset)) ll)
			   (cdr ll) (cons n (cdr ll))))
		((> offset 0)
		 (setf ll (nthcdr (1- offset) ll)
			   (cdr ll) (cons n (cdr ll))))
		(t (setf ll (nthcdr (- len 2) ll)
				 (cdr ll) (cons n (cdr ll)))))
  ll)

(defun neg-offset-adjust (len offset)
  (loop with a = offset
		if (< a 1)
		  do (incf a (1- len))
		else
		  return a))

(defun print-circle (ll n)
  (loop for i from 0 to n
		collect (nth i ll)))

(defun part1 (&optional (input *input*))
  (let* ((ll (parse-input input))
		 (circle-ll (circle-it (copy-list ll))))
	(loop for n in ll
		  
		  do (setf circle-ll (move n circle-ll n (length ll)))
		  do (format t "after move: ~a~%" (print-circle circle-ll 10))
		  )
	
	(format t "~a~%" (print-circle circle-ll 10))

	(loop if (= 0 (car circle-ll))
			return nil
		  else
			do (setf circle-ll (cdr circle-ll)))
	
	(format t "~a~%" (print-circle circle-ll 10))
	(format t "1000: ~a, 2000: ~a, 3000: ~a~%" (nth 1000 circle-ll) (nth 2000 circle-ll) (nth 3000 circle-ll))
	(+ (nth 1000 circle-ll) (nth 2000 circle-ll) (nth 3000 circle-ll))
	))
