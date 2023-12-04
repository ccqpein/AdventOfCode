(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day4.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day4_demo.input"))

(defun parse-input (input first-len)
  (loop for line in input
        for ll = (cl-ppcre:all-matches-as-strings
                  "(\\d+)"
                  line)
        collect (list (subseq ll 1 (1+ first-len))
                      (subseq ll (1+ first-len)))))

(defun day4 (input first-len)
  (let ((input (parse-input input first-len)))
    (loop for card in input
          for len =  (length
                      (intersection (car card)
                                    (cadr card)
                                    :test 'equal))
          for vv = (loop
                     for ind from 1 to (1- len)
                     sum (expt 2 (1- ind))
                     )
          sum (if (/= 0 len) (1+ vv) vv))))

(defun day4-2 (input first-len)
  (let* ((input (parse-input input first-len))
         (bucket (make-list (length input) :initial-element 1)))
    (loop
      for ind upfrom 0
      for card in input
      for len = (length
                 (intersection (car card)
                               (cadr card)
                               :test 'equal))
      do (loop
           for dup from (1+ ind) to (+ ind len)
           do (incf (nth dup bucket)
                    (nth ind bucket))))
    (apply #'+ bucket)))

;; (day4 *input* 10)
;; (day4 *input-demo* 5)
