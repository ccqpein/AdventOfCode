(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day1.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day1_demo.input"))

(defun day1 (&optional (input *input*))
  (let (left right)
    (loop for line in input
          for (a b) = (mapcar #'parse-integer (str:split "   " line))
          do (push a left)
          do (push b right))
    
    (setf left (sort left #'<))
    (setf right (sort right #'<))

    (loop for a in left
          for b in right
          sum (abs (- a b)))))

(defun day1-2 (&optional (input *input*))
  (let ((table (make-hash-table :test 'equal))
        left)
    (loop for line in input
          for (a b) = (mapcar #'parse-integer (str:split "   " line))
          do (push a left)
          do (if (gethash b table) (incf (gethash b table)) (setf (gethash b table) 1)))

    (loop for a in (reverse left)
          sum (* a (gethash a table 0)))))

