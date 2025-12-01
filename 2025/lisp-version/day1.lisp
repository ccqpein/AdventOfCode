(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day1.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day1_demo.input"))

(defun day1 (input start)
  (loop for line in input
        do (str:match line
             (("L" n)
              (setf start
                    (let ((n (- start (parse-integer n))))
                      (mod n 100))))
             (("R" n)
              (setf start
                    (let ((n (+ start (parse-integer n))))
                      (mod n 100)))))
        if (= start 0)
          count 1))

(defun go-left (start offset)
  (loop for n from 1 to offset
        do (decf start)
        if (zerop (mod start 100))
          count 1 into res
          and do (setf start 100)
        finally (return (values start res))))

(defun go-right (start offset)
  (loop for n from 1 to offset
        do (incf start)
        if (zerop (mod start 100))
          count 1 into res
          and do (setf start 0)
        finally (return (values start res))))

(defun day1-2 (input start)
  (loop for line in input
        with res = 0
        do (str:match line
             (("L" n)
              (multiple-value-bind (new-pos v)
                  (go-left start (parse-integer n))
                (setf start new-pos)
                (incf res v)))
             (("R" n)
              (multiple-value-bind (new-pos v)
                  (go-right start (parse-integer n))
                (setf start new-pos)
                (incf res v))))
        finally (return res)))
