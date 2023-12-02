;;; use this one
(ql:quickload '("str" "alexandria" "split-sequence"))

(defun read-file-by-line (filepath)
  "read file line by line, return a list of file"
  (uiop:read-file-lines filepath)
  )

(defstruct (hash-set (:conc-name set-)
                     (:copier nil))
  "hash set"
  (inner (make-hash-table :test 'equal) :type hash-table :read-only t))

(defun copy-hash-set (set)
  (make-hash-set :inner (alexandria:copy-hash-table (set-inner set) :test 'equal)))

(defun set-insert (set &rest eles)
  "insert elements inside hashset, side effect: change the set"
  (declare (hash-set set))
  (dolist (ele eles)
    (setf (gethash ele (set-inner set)) t)))

(defun set-get (set ele)
  (declare (hash-set set))
  (if (gethash ele (set-inner set)) ele nil))

(defun hash-set-difference (set other)
  "return set - other list"
  (loop for k being the hash-keys of (set-inner set)
        if (not (gethash k (set-inner other)))
          collect k))

(defun make-hash-set-from-list (l)
  "as the function name says"
  (let ((s (make-hash-set)))
    (apply #'set-insert s l)
    s))

(defun set-to-list (set)
  "as the function name say"
  (loop for k being the hash-keys of (set-inner set)
        collect k))

(defun set-union (set other)
  "union sets, return new set"
  (make-hash-set-from-list
   (append (loop for k being the hash-keys of (set-inner other)
                 collect k)
           (loop for k being the hash-keys of (set-inner set)
                 collect k))))

(defun set-count (set)
  (hash-table-count (set-inner set)))

(defun set-same (set other)
  (and (= (set-count set)
          (set-count other))
       (loop for k being the hash-keys of (set-inner set)
             when (not (gethash k (set-inner other)))
               return nil
             finally (return t))))

(defun list-of-sum-rest (l)
  "give list and make a list which every elements are the
all rest original elements sum"
  (loop
    with sum = 0
    for n in (reverse l)
    collect (incf sum n) into result
    finally (return (reverse result))))

(defun str-split-all (ls str &rest arg)
  "split str with k in ls
&key omit-nulls limit start end"
  (let ((result (list str)))
    (dolist (s ls result)
      (setf result
            (alexandria:flatten
             (mapcar (lambda (str)
                       (apply #'str:split s str arg))
                     result)))
      )))

(defun make-matrix-from-aoc (strl)
  "str from read-file-by-line, so the length of strl is line number"
  (let* ((line-num (length strl))
         (col-num (length (car strl)))
         (m (make-array (list line-num col-num))))
    (loop
      for line in strL
      for l-ind from 0
      do (loop
           for c across line
           for c-ind from 0
           do (setf (aref m l-ind c-ind) c)
           ))
    m
    ))

(defun nth-nest (l coorp)
  (loop for i in coorp
        when (or (< i 0) (> i (length l)))
          return nil
        do (setf l (nth i l))
        finally (return l)))

;; (defun %set-nth-nest (l coorp v)
;;   (cond
;;     ((not coorp) nil) ;; error
;;     ((= 1 (length coorp)) (setf (nth (car coorp) l) v))
;;     (t (set-nth-nest (nth (car coorp) l) (cdr coorp) v))))

;; (defsetf nth-nest %set-nth-nest)

;;:= need learn how to do this
;; (defsetf nth-nest (l coorp) (new-value)
;;   ;;`(set-nth-nest ,l ,coorp ,new-value)
;;   (print coorp)
;;   (let ((inner-coorp (eval coorp)))
;;     `(setf ,(loop
;;               for i in inner-coorp
;;               do (setf l `(nth ,i ,l))
;;               finally (return l))
;;            ,new-value)))
;;   )

;; (defmacro loop-array (dims syms &rest rest)
;;   (if (not dims)
;;       (cons 'progn rest)
;;       (let ((xn (car dims)))
;;         `(loop
;;            for ,(car syms) from 0 below ,xn
;;            do (loop-array ,(cdr dims) ,(cdr syms) ,@rest)))))


(defparameter *aoc-session* nil "aoc session")
(defparameter *aoc-year* (nth 5 (multiple-value-list (get-decoded-time))) "aoc year")

(defun download-input (day-num &key (session *aoc-session*) input-file-path)
  "Get the {day-num} input. Maybe write to input-file-path.
Need the session in cookie for authorizing."
  (let ((out (make-string-output-stream))
		content)
	(sb-ext:run-program "curl"
						(list "-sL"
							  "-H"
							  (format nil "cookie: session=~a" session)
							  (format nil "https://adventofcode.com/~a/day/~a/input" *aoc-year* day-num)
							  )
						:search t
						:output out)
	
	(setf content (get-output-stream-string out))
	
	(if input-file-path
		(with-open-file (s input-file-path :direction :output :if-does-not-exist :create)
		  (format s content)))
	
	content
	))

;; (download-input 1 :session "lalalalalal" :input-file-path "../aoc2022/inputs/day1.input")

(defun chunk-list (list n)
  "chunk list to a list of serveral n elements list"
  (loop
	with x = 0
	for end = (+ x n)
	if (< end (length list))
	  collect (subseq list x end) into new
	  and do (setf x end)
	else
	  collect (subseq list x) into new
	  and do (return new)
	end))

