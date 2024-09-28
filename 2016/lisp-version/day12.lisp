(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day12.input"))
(defparameter *input-demo*
  (str:lines "cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a"))

(defun handle-lines (lines table)
  (do* ((ind 0)
        (line (nth ind lines) (nth ind lines)))
       ((>= ind (length lines)))
    (str:match line
      (("cpy " x " " y)
       (setf (gethash y table) (handler-case (parse-integer x)
                                 (error (e)
                                   (declare (ignore e))
                                   (gethash-or-insert x table 0))))
       (incf ind)
       ;;(format t "copy ~a to ~a~%" x y)
       )
      (("inc " x)
       (setf (gethash x table) (1+ (gethash-or-insert x table 0)))
       (incf ind)
       ;;(format t "inc ~a to ~a~%" x (gethash x table))
       )
      (("dec " x)
       (setf (gethash x table) (1- (gethash-or-insert x table 0)))
       (incf ind)
       ;;(format t "dec ~a to ~a~%" x (gethash x table))
       )
      (("jnz " x " " y)
       (let (n)
         (handler-case (parse-integer x)
           (error (e)
             (declare (ignore e))
             (setf n (gethash-or-insert x table 0)))
           (:no-error (a b) (declare (ignore b)) (setf n a)))

         (if (/= n 0)
             (incf ind (parse-integer y))
             (incf ind)))))
    ))

(defun gethash-or-insert (key table init)
  (if (gethash key table)
      (gethash key table)
      (progn (setf (gethash key table) init)
             (gethash key table))))

(defun part1 (&optional (input *input*))
  (let ((table (make-hash-table :test 'equal)))
    (handle-lines input table)
    (gethash "a" table)))

(defun part2 (&optional (input *input*))
  (let ((table (make-hash-table :test 'equal)))
    (setf (gethash "c" table) 1)
    (handle-lines input table)
    (gethash "a" table)))
