(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day14.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day14_demo.input"))

(defun parse-input (input)
  (loop for line in input
        collect (str:match line
                  (("p=" c "," r " v=" cv "," rv) (mapcar #'parse-integer (list c r cv rv))))))

(defun move (coop speed col-len row-len)
  (let ((coop-new (mapcar #'+ coop speed)))
    (cond ((>= (first coop-new) col-len)
           (setf (first coop-new) (- (first coop-new) col-len)))
          ((< (first coop-new) 0)
           (setf (first coop-new) (+ (first coop-new) col-len))))
    (cond ((>= (second coop-new) row-len)
           (setf (second coop-new) (- (second coop-new) row-len)))
          ((< (second coop-new) 0)
           (setf (second coop-new) (+ (second coop-new) row-len))))
    coop-new))

(defun day14 (time col-len row-len &optional (input *input*))
  (let ((input (parse-input input))
        all-coops)
    (setf all-coops
          (loop for (c r cv rv) in input
                collect  (loop with coop = (list c r)
                               for s from 1 to time
                               do (setf coop (move coop (list cv rv) col-len row-len))
                               finally (return coop))))

    (let ((c-mid (floor (/ col-len 2)))
          (r-mid (floor (/ row-len 2)))
          one two three four)
      (loop for coop in all-coops
            do (cond ((and (< (first coop) c-mid)
                           (< (second coop) r-mid))
                      (push coop one))
                     ((and (> (first coop) c-mid)
                           (< (second coop) r-mid))
                      (push coop two))
                     ((and (< (first coop) c-mid)
                           (> (second coop) r-mid))
                      (push coop four))
                     ((and (> (first coop) c-mid)
                           (> (second coop) r-mid))
                      (push coop three))))
      (apply #'* (mapcar #'length (list one two three four))))))

;;(day14 100 11 7 *input-demo*)
;;(day14 100 101 103 *input*)

(defun one-line-check (ll row-len)
  (let ((table (make-hash-table :test 'equal)))
    (loop for l in ll
          do (push (first l) (gethash (second l) table)))
    (loop for r from 0 below row-len
          for nums = (gethash r table)
          when (count-continuous nums)
            return t
          finally (return nil))))

(defun count-continuous (l)
  (let ((ll (sort l #'<)))
    (do* ((rest ll (cdr rest))
          (a (car rest) (car rest))
          (b (cadr rest) (cadr rest))
          (count 0))
         ((or (not b) (> count 10))
          (> count 10))
      (if (= (- b a) 1)
          (incf count)
          (setf count 0)))))

(defun day14-2 (col-len row-len &optional (input *input*))
  (let ((input (parse-input input))
        (m (gen-aoc-map (loop for r from 1 to row-len collect (make-list col-len :initial-element "."))
                        :coop-ele t
                        :ele-coops t)))
    (loop with input = input
          for i upfrom 1 
          do (setf input
                   (loop for (c r cv rv) in input
                         for coop = (move `(,c ,r) (list cv rv) col-len row-len)
                         collect (list (first coop) (second coop) cv rv)))
          when (one-line-check input row-len)
            do (loop for (c r cv rv) in input
                     do (set-aoc-map-ele m `(,r ,c) "*"))
               (format t "time: ~a~%" i)
               (print-raw-map m)
            and return nil)))

;;(day14-2 101 103 *input*)

