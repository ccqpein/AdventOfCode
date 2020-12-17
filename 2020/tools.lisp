;;; use this one
(ql:quickload '("str" "alexandria" "split-sequence"))

(defun read-file-by-line (filepath)
  "read file line by line, return a list of file"
  (with-open-file (s filepath :direction :input)
    (do ((l (read-line s) (read-line s nil 'eof))
         (result '()))
        ((eq l 'eof) (reverse result))
      (push l result))))

(defstruct (hash-set (:conc-name set-))
  (inner (make-hash-table :test 'equal) :type hash-table :read-only t))

(defun set-insert (set &rest eles)
  (declare (hash-set set))
  (dolist (ele eles)
    (setf (gethash ele (set-inner set)) t)))

(defun set-get (set ele)
  (declare (hash-set set))
  (if (gethash ele (set-inner set)) ele nil))

(defun list-of-sum-rest (l)
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

(defmacro with-matrix ((row col m) form &body body)
  "row is row number, col is col number, m is matrix of form"
  (let ((dims (gensym)))
    `(let* ((,m ,form)
            (,dims (array-dimensions ,m))
            (,row (car ,dims))
            (,col (cadr ,dims))
            )
       ,@body
       )))
