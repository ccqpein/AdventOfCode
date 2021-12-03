(load "../../2020/tools.lisp")

(defun all-bits-zero-one (all-lines)
  (loop
    for i from 0 below (length (car all-lines))
    collect (special-bit-zero-one-sum all-lines i)))

(defun special-bit-zero-one-sum (all-lines b)
  (loop
    for line in all-lines
    if (char= #\0 (elt line b))
      count 1 into zero
    else
      count 1 into one
    finally (return (list zero one))))

(defun part1 (path)
  (let ((record (all-bits-zero-one (read-file-by-line path))))
    (loop
      for (zero one) in record
      collect (if (> zero one) "0" "1") into a
      collect (if (>= zero one) "1" "0") into b
      finally (return (* (parse-integer (apply 'str:concat a) :radix 2)
                         (parse-integer (apply 'str:concat b) :radix 2)))
      )))

(defun part2 (path)
  (let ((all-lines (read-file-by-line path)))
    (labels ((inner (all-lines update-value-func)
               (do ((ind 0 (1+ ind))
                    (a all-lines)
                    )
                   ((<= (length a) 1) (parse-integer (apply 'str:concat a) :radix 2))
                 (let* ((zero-one (special-bit-zero-one-sum a ind))
                        (v (funcall update-value-func zero-one)))
                   (setf a (remove-if-not (lambda (line) (char= (elt line ind) v)) a))
                   ))))

      (* (inner all-lines (lambda (z-o)
                            (if (> (car z-o) (cadr z-o)) #\0 #\1)))
         (inner all-lines (lambda (z-o)
                            (if (> (car z-o) (cadr z-o)) #\1 #\0)))))
    ))

;;(part1 "../src/bin/day3/day3.input")

