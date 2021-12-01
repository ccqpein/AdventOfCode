(load "../../2020/tools.lisp")

(defun part1 (path)
  (let ((result 0))
    (reduce
     (lambda (last this)
       (if (> this last)
           (incf result))
       this)
     (mapcar 'str::parse-integer (read-file-by-line path)))
    result))

(defun part1-v2 (path)
  (loop
    for (a b) on (mapcar 'str::parse-integer (read-file-by-line path))
    when (and a b)
      count (> b a)
    ))

(defun part2 (path)
  (loop
    with last = most-positive-fixnum
    and this = 0
    for a on (mapcar 'str::parse-integer (read-file-by-line path))
    when (>= (length a) 3)
      do (setf this (apply #'+ (subseq a 0 3)))
      and count (> this last) into result
      and do (setf last this)
    end 
    finally (return result)
    ))
