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
  (declare (integer s))
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

(defun opt-part2 (input)
  (declare (optimize speed (space 0)))
  (let ((table (make-hash-table :test 'equal)))
    (loop for line in input
          do (do* ((set (make-hash-set))
                   (a (parse-integer line) b)
                   (aa (mod a 10) bb)
                   (b (next-number a) c)
                   (bb (mod b 10) cc)
                   (c (next-number b) d)
                   (cc (mod c 10) dd)
                   (d (next-number c) e)
                   (dd (mod d 10) ee)
                   (e (next-number d) (next-number d))
                   (ee (mod e 10) (mod e 10))
                   (ind 4 (1+ ind)))
                  ((= ind 2000) nil)
               (declare (integer a b c d e)
                        (fixnum aa bb cc dd ee)
                        (optimize speed))
               (let* ((seq (loop for (x y) on (list aa bb cc dd ee) by #'cdr
                                 while y
                                 collect (- y x))))
                 (unless (set-get set seq)
                   (incf (the fixnum (gethash seq table 0)) ee)
                   (set-insert set seq)))))
    (loop for v being each hash-value of table
          maximize v)))

(defun day22 (input &optional part2)
  (if part2
      ;; (bf-part2 input)
      (opt-part2 input)
      (apply #'+ (mapcar (lambda (num) (next-n-number (parse-integer num) 2000)) input))))
