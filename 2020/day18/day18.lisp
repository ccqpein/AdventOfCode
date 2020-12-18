(load "../tools.lisp")

;; (defun cal (ls)
;;   ;;(print ls)
;;   (loop
;;     with result = (parse-integer (car ls))
;;     and op = #'+
    
;;     for w in (cdr ls)
;;     ;;do (print w)
;;     do (cond 
;;          ((string= w "+") (setf op #'+))
;;          ;;("-")
;;          ((string= w "*") (setf op #'*))
;;          (t (setf result (funcall op result (parse-integer w))))
;;          )
;;     finally (return (write-to-string result))))

;; (defun cal (ls)
;;   (loop
;;     with result and op
;;     for i in ls
;;     if (consp i)
;;       do (funcall op result (cal i))
;;     else
;;       do (cond ((string= i "+") (setf op #'+))
;;                ((string= i "*") (setf op #'*))
;;                (t (setf result (funcall op result (parse-integer i)))))
;;     finally (return (write-to-string result)))
;;   ls)

;; (defun cal (sum rest)
;;   (cond ((consp (car ls))
;;          (cal (cal (car ls)) (cdr rest)))
;;         ((string= "+" (car rest))
;;          (cal (+ sum ))
;;          )
;;         (t (cal (car rest) (cdr rest)))))

(defun cal (l)
  ;;(print l)
  (loop
    with op = nil
    with result
    for i in l
    for this = (cond ((consp i)
                      (cal i))
                     (t i))
    do (cond ((string= this "+") (setf op #'+))
             ((string= this "*") (setf op #'*))
             ((not op) (setf result (parse-integer this)))
             (t (setf result (funcall op result (parse-integer this))))
             )
    finally (return (write-to-string result))))

(defun cal-2 (l)
  (let ((ll (mapcar (lambda (x)
                      (if (consp x)
                          (cal-2 x)
                          x))
                    l)))
    ;;(print ll)
    (do ((this (car ll) (car rest))
         (rest (cdr ll) (cdr rest))
         (stack '()))
        ((not this)
         ;;(format t "stack:~a, rest: ~a~%" stack rest) 
         (cal stack))
      ;;(print rest)
      (if (string= "+" this)
          (progn (setf stack (cons (write-to-string
                                    (+ (parse-integer (car stack))
                                       (parse-integer (car rest))))
                                   (cdr stack))
                       rest (cdr rest)))
          (push this stack))
      )))

(defun para-run (ls)
  ;;(format t "ls: ~a~%" ls)
  (do ((this (car ls) (car rest))
       (rest (cdr ls) (cdr rest))
       (result '()))
      ((or (str:ends-with? ")" this) (not this))
       (progn
         (when this
           (let ((ss (str:split ")" this
                                :limit 2)))
             ;;(format t "ss: ~a~%" ss)
             (if (string/= (car ss) "")
                 (push (car ss) result))
             (if (string/= (cadr ss) "")
                 (setf rest (append (cdr ss) rest)))))
         (values (reverse result) rest)))
    ;;(format t "rest: ~a" rest)
    (if (str:starts-with? "(" this)
        (multiple-value-bind (a b)
            (para-run (append (list (str:s-rest this)) rest))
          (push a result)
          (setf rest b))
        (push this result))))

(defun parse (l fun)
  (let ((s (str:split " " l :omit-nulls t)))
    (funcall fun (para-run s))))

(defun day18 (fun)
  (let ((lines (read-file-by-line "./day18.input")))
    (apply #'+ (mapcar (lambda (d) (parse-integer d))
                       (loop for line in lines
                             ;;do (print line)
                             collect (parse (str:concat "(" line ")") fun))))
    ))

;; part1
(day18 #'cal)
;;part2
(day18 #'cal-2)
