(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day13.input"))
(defparameter *demo-input* (read-file-by-line "../inputs/day13_demo.input"))
(defparameter *demo-input-2* (read-file-by-line "../inputs/day13_demo_2.input"))


(defun day13 (&optional (input *input*))
  (let ((pairs (chunk-list input 3)))
	(loop for pair in pairs
		  for idx from 1 to (length pairs)
		  for left = (read-one-input (replace-input (first pair)))
		  for right = (read-one-input (replace-input (second pair)))
		  if (= 0 (compare left right))
			sum idx
		  )))

(defun day13-part2 (&optional (input *input*))
  (let ((inputs (mapcar #'read-one-input
						(mapcar #'replace-input
								(remove "" input :test 'string=))))
		sorted-input)
	
	(setf sorted-input
		  (sort (append inputs (list '((2)) '((6))))
				(lambda (a b) (= 0 (compare a b)))))

	(* (1+ (position '((2)) sorted-input :test 'equal))
	   (1+ (position '((6)) sorted-input :test 'equal)))
	))

(defun all-right (inputs)
  (every (lambda (e) (= 0 (compare (first e) (second e))))
		 (chunk-list inputs 2)))

(defun replace-input (input)
  (str:replace-all
   "," " "
   (str:replace-all
	"]" ")"
	(str:replace-all "[" "(" input))))

(defun read-one-input (input)
  (read-from-string input))

;; 0 right, 1 undecided, 2 not right
(defun compare (left right)
  (cond ((and (typep left 'integer) (typep right 'integer))
		 (cond ((< left right) 0) ((= left right) 1) (t 2)))
		
		((and (typep left 'cons) (typep right 'cons))
		 (loop
		   with l-len = (length left)
		   and r-len = (length right)
		   for l from 0 below l-len
		   for r from 0 below r-len
		   for c = (compare (nth l left) (nth r right))
		   when (/= 1 c)
			 return c
		   end
		   finally (return (cond ((< l-len r-len) 0) ((= l-len r-len) 1) (t 2)))
		   ))

		((typep left 'integer)
		 (compare (list left) right))

		((typep right 'integer)
		 (compare left (list right)))

		((and (typep left 'cons) (not right))
		 2)
		((and (typep right 'cons) (not left))
		 0)
		((and (not left) (not right))
		 1)
		))
