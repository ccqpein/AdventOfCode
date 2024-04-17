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
          do (format t "line: ~a~% ins: ~a~%" line ins)
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
    (if (= 1 (length whole)) (return-from gen-func (list "" nil op)))
    (setf v (str:substring 0 1 (car whole))
          o (str:substring 1 2 (car whole))
          num (parse-integer (str:substring 2 (length (car whole)) (car whole))))
    ;;(format t "~a ~a ~a ~a~%" v o num re)
    (list v (lambda (x) (if (cond ((string= o ">") (> x num))
                                  ((string= o "<") (< x num)))
                            re))
          re)))

(defun parse-data (data)
  (mapcar (lambda (a) (let ((a (str:split "=" a))) (cons (car a) (parse-integer (cadr a)))))
          (str:split ","
                     (car (remove-if (lambda (a)
                                       (string= "" a))
                                     (cl-ppcre:split "[{}]" data))))))

(defun run (table name vv)
  (loop for ins = (gethash name table)
        do (loop for (v f re) in ins
                 for va = (assoc v vv :test #'string=)
                 ;;do (format t "~a ~a ~a~%" v f va)
                 do (if va
                        (progn (setf name (funcall f (cdr va)))
                               (cond ((string= "A" name) (return-from run t))
                                     ((string= "R" name) (return-from run nil))
                                     ((string/= nil name) (return))))
                        (progn (setf name re)
                               (cond ((string= "A" name) (return-from run t))
                                     ((string= "R" name) (return-from run nil))))
                        ))))

(defun day19 (input)
  (multiple-value-bind (ins-table datas)
      (parse-input input)
    (loop for data in datas
          if (run ins-table "in" data)
            sum (apply #'+ (mapcar #'cdr data)))))

(defun split-list (f ll)
  (loop for x in ll
        if (funcall f x)
          collect x into y
        else
          collect x into n
        finally (return (values y n))))

(defun run2 (table name x m a s)
  (if (some (lambda (l) (= 0 (length l))) (list x m a s)) (return-from run2 nil))
  (if (string= name "A") (return-from run2 (* (length x) (length m) (length a) (length s))))
  (if (string= name "R") (return-from run2 nil))
  
  (let ((ins (gethash name table)))
    (let (next-x next-m next-a next-s)
      (loop for (vn f next-name) in ins
            for result = nil
            if (string/= vn "")
              do (setf result
                       (cond ((string= vn "x")
                              (multiple-value-bind (next-x new-x)
                                  (split-list f x)
                                (setf x new-x)
                                (run2 table next-name next-x m a s)))
                             ((string= vn "m")
                              (multiple-value-bind (next-m new-m)
                                  (split-list f m)
                                (setf m new-m)
                                (run2 table next-name x next-m a s)))
                             ((string= vn "a")
                              (multiple-value-bind (next-a new-a)
                                  (split-list f a)
                                (setf a new-a)
                                (run2 table next-name x m next-a s)))
                             ((string= vn "s")
                              (multiple-value-bind (next-s new-s)
                                  (split-list f s)
                                (setf s new-s)
                                (run2 table next-name x m a next-s)))
                             ))
            else
              do (setf result (run2 table next-name x m a s))
            if result
              sum result)
      )))

(defun day19-2 (input)
  (multiple-value-bind (ins-table datas)
      (parse-input input)
    (let ((4kl (loop for i from 1 to 4000 collect i)))
      (run2 ins-table "in" 4kl 4kl 4kl 4kl))))

;;(= 368964 (day19 *input*))
;;(= 127675188176682 (day19-2 *input*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; try to use macro
;;; https://www.reddit.com/r/Common_Lisp/comments/1c5uq3k/advanced_users_advent_of_code_2023_days_1920
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clean-one-line-input (line)
  (let* ((whole (cl-ppcre:split "[{}]" line))
         (name (car whole))
         (second-part (cadr whole)))
    (list
     (read-from-string name)
     (loop for x in (str:split "," second-part)
           collect (str:match x
                     ((v "<" num ":" next)
                      `(< ,(read-from-string v) ,(read-from-string num) #',(read-from-string next)))
                     ((v ">" num ":" next)
                      `(> ,(read-from-string v) ,(read-from-string num) #',(read-from-string next)))
                     ((v)
                      `(#',(read-from-string v))))))))

;; (clean-one-line-input "px{a<2006:qkq,m>2090:A,rfg}")
;; => (PX ((< A 2006 #'QKQ) (< M 2090 #'A) (#'RFG)))

(defmacro gen-fun2 (cleaned-one-line)
  `(defun ,(car cleaned-one-line) (x m a s)
     ,@(loop for ins in (cadr cleaned-one-line)
             collect (if (> (length ins) 1)
                         `(if ,(subseq ins 0 3) (return-from ,(car cleaned-one-line) (apply ,(car (last ins)) x m a s)))
                         `(apply ,(car (last ins)) x m a s)
                         ))))

(pprint (macroexpand-1 `(gen-fun2 ,(clean-one-line-input "px{a<2006:qkq,m>2090:A,rfg}"))))
(pprint (macroexpand-1 `(gen-fun2 ,(clean-one-line-input "pv{a>1716:R,A}"))))
(pprint (macroexpand-1 `(gen-fun2 ,(clean-one-line-input "rfg{s<537:gd,x>2440:R,A}"))))
(pprint (macroexpand-1 `(gen-fun2 ,(clean-one-line-input "lnx{m>1548:A,A}"))))
