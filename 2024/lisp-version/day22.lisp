(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day22.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day22_demo.input"))

(defun mixing (s given)
  (logxor s given))

(defun pruning (s)
  (mod s 16777216))

(defun next-number (s)
  (let ((ss s))
    (setf ss (pruning (mixing ss (* ss 64))))
    (setf ss (pruning (mixing ss (floor (/ ss 32)))))
    (setf ss (pruning (mixing ss (floor (* ss 2048)))))
    ss))

(defun next-n-number (s time)
  (loop repeat time
        do (setf s (next-number s))
        finally (return s)))

(defun get-sequence (s time)
  (loop with last = s
        and last-digit = (mod s 10)
        repeat (1- time)
        for a = (next-number last)
        collect (- (mod a 10) last-digit) into digits
        collect (mod a 10) into values
        do (setf last a
                 last-digit (mod a 10))
        finally (return (values values digits))))

(defun bf-part2 (input)
  (let ((table (make-hash-table :test 'equal)))
    (loop for line in input
          for this-line-t = (multiple-value-bind (vs ds)
                                (get-sequence (parse-integer line) 2000)
                              (loop with this-table = (make-hash-table :test 'equal)
                                    for (a b c d) on ds by #'cdr
                                    while d
                                    for ind upfrom 3
                                    do (push (nth ind vs) (gethash (list a b c d) this-table nil))
                                    finally (return this-table)))
          do (loop for s being each hash-key of this-line-t
                     using (hash-value vs)
                   do (incf (gethash s table 0) (car (last vs)))))
    
    (apply #'max (alexandria:hash-table-values table))))

(defun day22 (input &optional part2)
  (if part2
      (bf-part2 input)
      (apply #'+ (mapcar (lambda (num) (next-n-number (parse-integer num) 2000)) input))))
