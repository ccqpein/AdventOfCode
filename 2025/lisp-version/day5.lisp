(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day5.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day5_demo.input"))

(defun day5 (input)
  (let* ((ss (split-sequence:split-sequence-if (lambda (s) (string= s "")) input))
         (ranges (first ss))
         (all-ids (second ss))
         all-checkers )
    (setf all-checkers
          (loop for line in ranges
                do (print line)
                collect (str:match line
                          ((a "-" b) (lambda (n) (<= (parse-integer a) n (parse-integer b)))))))
    
    (- (length all-ids)
       (loop for id in all-ids
             unless (some (lambda (f) (funcall f (parse-integer id)))
                          all-checkers)
               count 1))))

(defun day5-2 (input)
  (let* ((ss (split-sequence:split-sequence-if (lambda (s) (string= s "")) input))
         (ranges (first ss))
         (sorted-ranges (sort (loop for line in ranges
                                    collect (str:match line
                                              ((a "-" b) `(,(parse-integer a)
                                                           ,(parse-integer b)))))
                              #'< :key #'car)))
    (do* ((all-ranges (cdr sorted-ranges) (cdr all-ranges))
          (this (first sorted-ranges))
          (next (first all-ranges) (first all-ranges))
          (res 0))
         ((not this) res)
      ;;(format t "~a ~a~%" this next)
      (cond ((not next)
             (incf res (- (second this) (first this) -1))
             (setf this nil))

            ((<= (second next) (second this)) 
             (setf this this))

            ((> (first next) (second this))
             (setf res (+ res (- (second this) (first this) -1))
                   this next))

            ((<= (first next) (second this))
             (setf this `(,(first this) ,(second next))))))))
