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

(defun move-2 (ll)
  (loop with has-moved = (make-hash-set)
		and len = (length ll)
		for i from 0 below len
		;;do (print i)
		if (not (set-get has-moved (nth i ll)))
		  do (let ((new-ind (mod (+ i (nth i ll)) (1- len)))
				   (n (nth i ll)))
			   (if (< new-ind 0)
				   (incf new-ind (1- (length ll))))
			   (format t "new ind: ~a ~a~%" new-ind n)
			   (setf ll (remove-l i ll))
			   ;;(format t "after move: ~a~%" ll)
			   (set-insert has-moved n) ;; wrong 
			   
			   (setf ll (insert-l new-ind ll n))
			   ;;(format t "after insert: ~a~%" (length ll))
			   (decf i)
			   )
		finally (return ll)
		))

(defun insert-l (n ll ele)
  ;;(format t "n: ~a, ll len: ~a, ele: ~a~%" n (length ll) ele)
  (if (= 0 n)
	  (cons ele ll)
	  (progn (setf (cdr (nthcdr (1- n) ll)) (cons ele (nthcdr n ll)))
			 ll)))

(defun remove-l (n ll)
  (if (= 0 n)
	  (cdr ll)
	  (progn (setf (cdr (nthcdr (1- n) ll))
				   (cddr (nthcdr (1- n) ll)))
			 ll))
  )

(defun neg-offset-adjust (len offset)
  ;; (loop with a = offset
  ;; 		if (< a 1)
  ;; 		  do (incf a (1- len))
  ;; 		else
  ;; 		  return a)
  (mod offset (1- len))
  )

(defun print-circle (ll n)
  (loop for i from 0 to n
		collect (nth i ll)))

(defun part1 (&optional (input *input*))
  (let* ((ll (parse-input input))
		 ;;(circle-ll (circle-it (copy-list ll)))
		 circle-ll
		 )
	;; (loop for n in ll		  
	;; 	  do (setf circle-ll (move n circle-ll n (length ll)))
	;; 	  do (format t "after move: ~a~%" (print-circle circle-ll 10))
	;; 	  )
	
	;; (format t "~a~%" (print-circle circle-ll 10))

	;; (loop if (= 0 (car circle-ll))
	;; 		return nil
	;; 	  else
	;; 		do (setf circle-ll (cdr circle-ll)))

	(setf circle-ll (circle-it (move-2 ll)))

	(loop if (= 0 (car circle-ll))
			return nil
		  else
			do (setf circle-ll (cdr circle-ll)))
	
	;;(format t "~a~%" (print-circle circle-ll 10))
	(format t "1000: ~a, 2000: ~a, 3000: ~a~%" (nth 1000 circle-ll) (nth 2000 circle-ll) (nth 3000 circle-ll))
	(+ (nth 1000 circle-ll) (nth 2000 circle-ll) (nth 3000 circle-ll))
	))
