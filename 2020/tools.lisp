;;; use this one
(ql:quickload '("str" "alexandria" "split-sequence"))

(defun read-file-by-line (filepath)
  "read file line by line, return a list of file"
  (uiop:read-file-lines filepath)
  )

(defstruct (hash-set (:conc-name set-)
                     (:copier copy-hash-set))
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
  (loop for i in coorp when (or (< i 0) (> i (length l))) return nil do (setf l (nth i l))
        finally (return l)))

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
