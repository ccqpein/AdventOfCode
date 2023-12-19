(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day19.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day19_demo.input"))

(defun parse-input (input)
  (let* ((input (split-sequence:split-sequence-if (lambda (a) (string= "" a)) input))
         (first (car input))
         (second (cadr input))
         (ins-table (make-hash-table :test 'equal)))
    (loop for line in first
          for ins = (parse-ins line)
          do (setf (gethash (car ins) ins-table) (cdr ins)))
    (values ins-table (mapcar #'parse-data second))))

(defun parse-ins (line)
  (let* ((whole (cl-ppcre:split "[{}]" line))
         (name (car whole))
         (ops (cadr whole)))
    (cons name
          (loop for op in (str:split "," ops)
                collect (gen-func op)))))

(defun gen-func (op)
  (let* ((whole (str:split ":" op))
         v o num
         (re (cadr whole)))
    (if (= 1 (length whole)) (return-from gen-func (list "" op)))
    (setf v (str:substring 0 1 (car whole))
          o (str:substring 1 2 (car whole))
          num (parse-integer (str:substring 2 (length (car whole)) (car whole))))
    ;;(format t "~a ~a ~a ~a~%" v o num re)
    (list v (lambda (x) (if (cond ((string= o ">") (> x num))
                                  ((string= o "<") (< x num)))
                            re)))
    ))

(defun parse-data (data)
  (mapcar (lambda (a) (let ((a (str:split "=" a))) (cons (car a) (parse-integer (cadr a)))))
          (str:split ","
                     (car (remove-if (lambda (a)
                                       (string= "" a))
                                     (cl-ppcre:split "[{}]" data))))))

(defun run (table name vv)
  (loop for ins = (gethash name table)
        do (loop for (v f) in ins
                 for va = (assoc v vv :test #'string=)
                 ;;do (format t "~a ~a ~a~%" v f va)
                 do (if va
                        (progn (setf name (funcall f (cdr va)))
                               (cond ((string= "A" name) (return-from run t))
                                     ((string= "R" name) (return-from run nil))
                                     ((string/= nil name) (return))))
                        (progn (setf name f)
                               (cond ((string= "A" name) (return-from run t))
                                     ((string= "R" name) (return-from run nil))))
                        ))))

(defun day19 (input)
  (multiple-value-bind (ins-table datas)
      (parse-input input)
    (loop for data in datas
          if (run ins-table "in" data)
            sum (apply #'+ (mapcar #'cdr data)))
    ))
