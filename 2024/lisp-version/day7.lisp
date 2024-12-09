(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day7.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day7_demo.input"))

(defun parse-input (inputs)
  (loop for line in inputs
        collect (str:match line
                  ((target ": " rest)
                   (cons (parse-integer target) (mapcar #'parse-integer (str:words rest)))))))

(defun cal-3 (l)
  (if (= 1 (length l)) (return-from cal-3 l))
  ;;(format t "~a~%" l)
  (loop for n in (cal-3 (cdr l))
        collect (+ (car l) n) into a
        collect (* (car l) n) into b
        finally (return (append a b))))

(defun cal-4 (l)
  (if (= 1 (length l)) (return-from cal-4 l))
  ;;(format t "~a~%" l)
  (loop for n in (cal-4 (cdr l))
        collect (+ (car l) n) into a
        collect (* (car l) n) into b
        collect (parse-integer (format nil "~a~a" n (car l))) into c
        finally (return (append a b c))))

(defun day7 (&optional part2 (input *input*))
  (loop for line in (parse-input input)
        when (member (car line)
                     (if part2
                         (cal-4 (reverse (cdr line)))
                         (cal-3 (reverse (cdr line)))))
          sum (car line)))

;; re-write with new reduce-all-operations below

(defun cal-3 (l)
  (reduce-all-operations l (list #'+ #'*)))

(defun cal-4 (l)
  (reduce-all-operations l (list #'+ #'* (lambda (a b) (parse-integer (format nil "~a~a" a b))))))

(defun day7-new (&optional part2 (input *input*))
  (loop for line in (parse-input input)
        when (member (car line)
                     (if part2
                         (cal-4 (cdr line))
                         (cal-3 (cdr line))))
          ;;do (pprint line)
          sum (car line)))
