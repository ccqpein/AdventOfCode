(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day15.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day15_demo.input"))

(defun parse-input (input)
  (loop for line in input
        collect (mapcar (lambda (s) (concatenate 'list s)) (split-sequence:split-sequence #\, line))))

(defun parse-input-2 (input)
  (loop for line in input
        collect (loop for word in (split-sequence:split-sequence #\, line)
                      for (label len) = (str:split "=" word)
                      collect (list label len))))

(defun get-hash (word)
  (loop
    with current-v = 0
    for c in word
    do (incf current-v (char-code c))
    do (setf current-v (* 17 current-v))
    do (setf current-v (mod current-v 256))
    finally (return current-v)))

(defun day15 (input)
  (let ((input (parse-input input)))
    (loop
      for line in input
      sum (loop
            for word in line
            sum (get-hash word)
            ))))

(defun handle (table-box label len)
  (let (label-hash)
    (if (not len)
        (setf label-hash (get-hash (butlast (concatenate 'list label))))
        (setf label-hash (get-hash (concatenate 'list label)))
        )
    (if len
        (let ((box-vs (gethash label-hash table-box)))
          (loop with flag = nil
                for v in box-vs
                if (string= label (car v))
                  collect (list label len) into new-vs
                  and do (setf flag t)
                else
                  collect v into new-vs
                finally (progn (if (not flag) (setf new-vs (append new-vs (list (list label len)))))
                               (setf (gethash label-hash table-box) new-vs))))
        (let ((box-vs (gethash label-hash table-box)))
          (loop for v in box-vs
                if (string/= (subseq label 0 (1- (length label))) (car v))
                  collect v into new-vs
                finally (setf (gethash label-hash table-box) new-vs))))
    ))

(defun day15-2 (input)
  (let ((input (parse-input-2 input))
        (table-box (make-hash-table :test 'equal))
        )
    (loop for line in input
          do (loop for (label len) in line
                   do (handle table-box label len)))

    (loop for box being the hash-keys of table-box
          using (hash-value values)
          sum (loop for (lb v) in values
                    for order upfrom 1
                    ;;do (print (* (1+ box) (parse-integer (cadr v)) order))
                    sum (* (1+ box) (parse-integer v) order)))))
