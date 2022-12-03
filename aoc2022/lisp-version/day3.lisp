(load "../../tools/tools.lisp")

(defun half-split-string (s)
  (let ((len (length s)))
	(list (subseq s 0 (/ len 2))
		  (subseq s (/ len 2) len))))

(defun count-the-damn-point (char)
  (if (lower-case-p char)
	  (- (char-code char) 96)
	  (- (char-code char) 38)))

(defun day3 ()
  (let ((input (read-file-by-line "../inputs/day3.input")))
	(loop
	  for line in input
	  for (a b) = (half-split-string line)
	  sum (count-the-damn-point (car (intersection (concatenate 'list a)
												   (concatenate 'list b)))))))

(defun day3-part2 ()
  (let ((input (read-file-by-line "../inputs/day3.input")))
	(loop
	  for (a b c) in (chunk-list input 3)
	  sum (count-the-damn-point
		  (car
		   (intersection
			(intersection (concatenate 'list a)
						  (concatenate 'list b))
			(concatenate 'list c)))))))

