(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day10.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day10_demo.input"))

(defun parse-input (input)
  (let (body s)
    (setf body
          (loop for line in input
                for row upfrom 0
                append (loop for char in (concatenate 'list line)
                             for col upfrom 0
                             if (char/= #\. char)
                               do (if (char= #\S char) (setf s (list row col)))
                               and collect (list char row col)
                             )))
    (values s body)))

(defun gen-table (parsed-input)
  (let ((table (make-hash-table :test 'equal)))
    (loop for x in parsed-input do (setf (gethash (cdr x) table) (gen-dir (car x) (cdr x))))
    table))

(defun gen-dir (char this)
  (let ((d (ccase char
             (#\| (list '(1 0) '(-1 0)))
             (#\- (list '(0 1) '(0 -1)))
             (#\L (list '(-1 0) '(0 1)))
             (#\J (list '(-1 0) '(0 -1)))
             (#\7 (list '(0 -1) '(1 0)))
             (#\F (list '(1 0) '(0 1)))
             (#\S (list '(1 0) '(0 1) '(-1 0) '(0 -1))))))
    (loop for (ro co) in d collect (list (+ ro (car this)) (+ co (cadr this))))))

(defun pre-start (start table)
  (loop for neighbour in (gethash start table)
        if (member start (gethash neighbour table) :test 'equal)
          collect neighbour))

(defun next-step (table from this)
  (car (remove from (gethash this table) :test 'equal)))

(defun day10 (input)
  (multiple-value-bind (s-loc input)
      (parse-input input)
    
    (let* ((table (gen-table input))
           (ss (pre-start s-loc table)))

      (setf (gethash s-loc table) ss)
      
      (do ((way-one (list s-loc (car ss)))
           (way-two (list s-loc (cadr ss)))
           (step 1 (1+ step)))
          ((equal (cdr way-one) (cdr way-two)) step)
        
        (setf way-one (cdr (append way-one (list (apply #'next-step table way-one)))))
        
        (setf way-two (cdr (append way-two (list (apply #'next-step table way-two))))))
      )))

(defun part2-dir-helper (ll table)
  (let ((start (nth 0 ll))
        (end (nth 2 ll)))
    (setf (gethash (nth 1 ll) table)
          (cond ((> (- (car end) (car start)) 0) 1)
                ((< (- (car end) (car start)) 0) -1)
                (t 0)))))

(defun scan-map (table row-len col-len)
  (loop for r from 0 below row-len
        sum (loop with status = 0
                  and last = 0
                  for c from 0 to col-len
                  if (and (not (gethash (list r c) table)) (/= 0 status))
                    sum 1
                  else
                    do (let ((v (gethash (list r c) table)))
                         (when (and v (/= v last))
                           (progn (incf status v)
                                  (if (/= status 0) (setf status (/ status (abs status))))
                                  (if (/= 0 v) (setf last v))))))))

(defun day10-2 (input)
  (let ((row-len (length input))
        (col-len (length (car input))))
    (multiple-value-bind (s-loc input)
        (parse-input input)
      (let* ((table (gen-table input))
             (ss (pre-start s-loc table))
             (table-2 (make-hash-table :test 'equal)))

        (setf (gethash s-loc table) ss)
      
        (do ((way-one (list s-loc (car ss)))
             (flag nil t))
            ((and flag (equal (car way-one) s-loc))
             (scan-map table-2 row-len col-len))
          
          (setf way-one (append way-one (list (apply #'next-step table way-one))))
          
          (part2-dir-helper way-one table-2)
          
          (setf way-one (cdr way-one)))))))
