(load "./general.lisp")

(defun read-as-string (filepath)
  (with-open-file (s filepath)
	(read-line s)))

(defun handle-stack (rest stack metadata-num-l result)
  )

(defun day8 (filepath)
  (let ((all-input (mapcar #'parse-integer
						   (cl-ppcre:split " "
										   (read-as-string filepath)))))
	(do ((rest all-input)
		 (stack '())
		 (metadata-num-l '())
		 (result 0))
		((not rest) result)
	  ;;(print "inside do") (print stack) (print metadata-num-l) (print result) (print rest)
	  (if (= 0 (car rest))
		  (setf result (+ result
						  (apply #'+
								 (subseq (cddr rest) 0 (cadr rest))))
				rest (subseq (cddr rest) (cadr rest))
				stack (cons (- (car stack) 1) (cdr stack)))
		  (setf stack (cons (car rest) stack)
				metadata-num-l (cons (cadr rest) metadata-num-l)
				rest (cddr rest)))

	  ;;(print "then") (print stack) (print metadata-num-l) (print result) (print rest)

	  (do ()
		 ((not (and (= 0 (car stack)) (> (length metadata-num-l) 0) (> (length stack) 1))))
	    (setf  result (+ result
							(apply #'+
								   (subseq rest 0 (car metadata-num-l))))
				  rest (subseq rest (car metadata-num-l))
				  metadata-num-l (cdr metadata-num-l)
				  stack (cons (- (cadr stack) 1) (cddr stack))
				  )
	    ;;(print "in while") (print stack) (print metadata-num-l) (print result) (print rest)
		)

	  (if (= 0 (car stack))
		  (setf  result (+ result
							(apply #'+
								   rest))
				  rest '()
				  metadata-num-l (cdr metadata-num-l)))
	  
	  ;;(print "after") (print stack) (print metadata-num-l) (print result) (print rest)
	  )))


;; learn new stuff, this macro cannot work in:
;; (let ((a (list 1 (list 2 3))) (ind (list 1 0))) (insert-stack-position a ind 2))
;; cause ind is lexical value, which cannot be eval in compile time. Which I guess is time of
;; expand macro
;; But, this will be work:
;; (defvar *ind* '(1 0)) (let ((a (list 1 (list 2 3)))) (insert-stack-position a *ind* 2))
(defmacro insert-stack-position (l inds ele)
  `(setf ,(loop
			 with temp = l
			 for i in (eval inds)
			 ;;do (print inds)
			 do (setf temp `(nth ,i ,temp))
			 finally (return temp))
		 ,ele))


;;; branch = (list of this (list of child branch))
(defun get-branch (l)
  (print l)
  (cond
	((not l) '())
	((= 0 (car l))
	 (list (cons (subseq l 0 (+ 2 (cadr l))) '())
			   (get-branch (subseq l (+ 2 (cadr l))))))
	(t
	 (list (append (subseq l 0 2)
				   (subseq l (- (length l) (cadr l))))
		   (get-branch (subseq l 2 (- (length l) (cadr l))))))))


(defun get-branch-2 (l)
  (print l)
  (cond
	((not l) '())
	((= 0 (car l))
	 (list (cons (subseq l 0 (+ 2 (cadr l))) '())
			   (get-branch (subseq l (+ 2 (cadr l))))))
	(t
	 (list (append (subseq l 0 2)
				   (cut-leave (cddr l)))
		   (get-branch (subseq l 2 (- (length l) (cadr l))))))))


(defun cut-leave (l))


(defun get-value-child (l)
  (if (= 0 (caar l))
	  (apply #'+ (cddar l))
	  (loop
		 for i in (cddar l)
		 sum (cond
			  ((not (nth (1- i) (cadr l)))
			   0)
			  (t
			   (get-value-child (nth (1- i) (cadr l))))))))


;;; part2 is not work, I should use object language to write this
(defun day8-part2 (filepath)
  (let* ((all-input (mapcar #'parse-integer
							(cl-ppcre:split " "
											(read-as-string filepath))))
		 (tree (get-branch all-input)))
	;;(get-value-child tree)
	tree
	))
