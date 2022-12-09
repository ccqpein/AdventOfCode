(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day9.input"))
(defparameter *demo-input* (list "R 4" "U 4" "L 3" "D 1" "R 4" "D 1" "L 5" "R 2"))
(defparameter *demo-input-2* (list "R 5" "U 8" "L 8" "D 3" "R 17" "D 10" "L 25" "U 20"))

(defun distance (head tail)
  (+ (abs (- (car head) (car tail)))
	 (abs (- (cadr head) (cadr tail)))))

(defun same-row-or-col (head tail)
  (or (= (car head) (car tail))
	  (= (cadr head) (cadr tail))))

(defun diagonally (head tail)
  (and (= 1 (abs (- (car head) (car tail))))
	   (= 1 (abs (- (cadr head) (cadr tail))))))

(defun around-p (head tail)
  (or (diagonally head tail)
	  (and (same-row-or-col head tail) (= 1 (distance head tail)))
	  (equal head tail)))

(defun day9 ()
  (let* ((input *input*)
		 (all-steps (loop for line in input
						  append (change-steps line)))
		 (set (make-hash-set)))
	(loop
	  with head = '(0 0)
	  and tail = '(0 0)
	  for s from 0 to (1- (length all-steps))
	  do (progn
		   (setf head (move head (nth s all-steps)))
		   (if (not (around-p head tail))
			   (setf tail (move-close head tail)))
		   (set-insert set tail)
		   ))
	(set-count set)
	))

(defun move (coop comm)
  (cond ((string= "R" comm)
		 (list (car coop) (1+ (cadr coop))))
		((string= "L" comm)
		 (list (car coop) (1- (cadr coop))))
		((string= "U" comm)
		 (list (1+ (car coop)) (cadr coop)))
		((string= "D" comm)
		 (list (1- (car coop)) (cadr coop)))))

(defun day9-part2 ()
  (let* ((input *input*)
		 (all-steps (loop for line in input
						  append (change-steps line)))
		 (set (make-hash-set))
		 (snake (make-list 10 :initial-element '(0 0))))
	(loop
	  for s from 0 to (1- (length all-steps))
	  do (progn
		   (setf (nth 0 snake) (move (nth 0 snake) (nth s all-steps)))
		   (loop for i from 1 to 9
				 do (if (not (around-p (nth (1- i) snake) (nth i snake)))
						(setf (nth i snake) (move-close (nth (1- i) snake) (nth i snake))))
				 )
		   ;;(print snake)
		   (set-insert set (nth 9 snake))
		   ))
	(set-count set)
	))

(defun move-close (head tail)
  "tail move close to head"
  (let (row-offset col-offset)
	(cond ((> (- (car head) (car tail)) 0)
		   (setf row-offset 1))
		  ((< (- (car head) (car tail)) 0)
		   (setf row-offset -1))
		  (t (setf row-offset 0)))

	(cond ((> (- (cadr head) (cadr tail)) 0)
		   (setf col-offset 1))
		  ((< (- (cadr head) (cadr tail)) 0)
		   (setf col-offset -1))
		  (t (setf col-offset 0)))
	
	;; return new tail
	(list (+ row-offset (car tail))
		  (+ col-offset (cadr tail))))
  )
