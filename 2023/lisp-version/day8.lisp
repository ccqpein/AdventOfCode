(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day8.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day8_demo.input"))

(defun parse-input (input)
  (append (list (car input))
          (loop for line in (cddr input)
                collect (cl-ppcre:all-matches-as-strings "\\w+" line))))

(defun gen-ins-and-table (parsed-input)
  (let (ins
        (table (make-hash-table :test 'equal)))
    (setf ins (concatenate 'list (car parsed-input))
          (cdr (last ins)) ins)

    (loop for (key l r) in (cdr parsed-input)
          do (setf (gethash key table) (list l r)))

    (values ins table)
    ))

(defun day8 (input)
  (let* ((input (parse-input input)))    
    (multiple-value-bind (ins table)
        (gen-ins-and-table input)
      (loop
        with here = "AAA"
        for step upfrom 1
        for i in ins
        do (cond ((char= i #\L)
                  (setf here (car (gethash here table))))
                 ((char= i #\R)
                  (setf here (cadr (gethash  here table)))))
        when (str:ends-with-p "Z" here)
          return step
        ))))

(defun day8-2 (input)
  (let ((input (parse-input input)))
    (multiple-value-bind (ins table)
        (gen-ins-and-table input)
      (let (all-A)
        (setf all-A
              (loop for (key l r) in (cdr input)
                    if (str:ends-with-p "A" key)
                      collect key))
        (apply #'lcm
               (loop for a in all-a
                     collect (loop
                               with here = a
                               for step upfrom 1
                               for i in ins
                               do (cond ((char= i #\L)
                                         (setf here (car (gethash here table))))
                                        ((char= i #\R)
                                         (setf here (cadr (gethash  here table)))))
                               when (str:ends-with-p "Z" here)
                                 return step))) ))))
