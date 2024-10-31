(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day20.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day20_demo.input"))

(defun parse-line (line)
  (str:match line
    ((first "-" second)
     (values (parse-integer first) (parse-integer second)))))

(defun day20 (&optional (input *input*))
  (let ((parsed-inputs (sort (loop for line in input
                                   collect (multiple-value-list (parse-line line)))
                             #'<
                             :key #'car)))
    ;;(format t "~{~a~^~%~}" (subseq parsed-inputs 0 10))
    (loop for (a b) on parsed-inputs
          if (not b)
            return (1+ (second a))
          else
            if (< (second a) (1- (first b)))
              return (1+ (second a)))
    ))

(defun day20-2 (&optional (input *input*))
  (let ((parsed-inputs (sort (loop for line in input
                                   collect (multiple-value-list (parse-line line)))
                             #'<
                             :key #'car)))

    ;;(format t "~{~a~^~%~}" (subseq parsed-inputs 0))
    (let ((result))
      (setf result
            (loop with a = (first parsed-inputs)
                  for b in (cdr parsed-inputs)
                  do (format t "~a ~a~%" a b)
                  if (< (second a) (1- (first b)))
                    collect a into result
                    and do (setf a b)
                  else
                    do (setf (second a) (max (second a) (second b)))
                  finally (progn (setf result (append result (list a)))
                                 (return result))))
      ;;(format t "~a~%" result)
      (loop for (a b) on (append result
                                 ;;'((10 10)) ;; demo
                                 '((4294967296 4294967296))
                                 )
            when b
              sum (- (first b) (second a) 1)
            ;; and do (print b)
            ))))
