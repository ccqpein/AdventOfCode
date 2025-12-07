(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day6.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day6_demo.input"))

(defun parse-input (input)
  (let ((lines (loop for line in input
                     collect (remove "" (str:split " " line) :test #'string=))))
    (loop for i from 0 below (length (car lines))
          collect (mapcar (lambda (line) (nth i line)) lines))))

(defun parse-input-2 (input)
  (let ((line-num (length input))
        output)
    (do ((ind 0 (1+ ind))
         cache
         res)
        ((= ind (length (car input)))
         (push cache res)
         (setf output res))
      (cond ((every (lambda (line) (char= #\  (elt line ind))) (butlast input))
             (push cache res)
             (setf cache nil))
            (t (push (apply #'str:concat
                            (mapcar #'string
                                    (loop for l-ind from 0 below (1- line-num)
                                          collect (elt (nth l-ind input) ind))))
                     cache))))
    (loop for op in (reverse (remove "" (str:split " " (car (last input))) :test #'string=))
          for o in output
          collect (append o (list op)))))

(defun cal (x)
  (cond ((string= "+" (car (last x)))
         (apply #'+ (mapcar #'parse-integer (butlast x))))
        ((string= "*" (car (last x)))
         (apply #'* (mapcar #'parse-integer (butlast x))))))

(defun day6 (input &optional part2)
  (let ((parsed-input (if part2
                          (parse-input-2 input)
                          (parse-input input))))
    (apply #'+ (mapcar #'cal parsed-input))))


(defun day6-2-new-way (input)
  (let ((m (gen-aoc-map input
                        :line-op (lambda (l) (concatenate 'list l))
                        :ele-frequency t :ele-coops t :coop-ele t)))
    ;; transpose the input (as map)
    (setf m (transpose-aoc-map m))

    (do* ((row-ind 0 (1+ row-ind))
          (row (mapcar #'string (get-aoc-map-row m row-ind))
               (mapcar #'string (get-aoc-map-row m row-ind)))
          op
          cache
          (res 0))
         ((= row-ind (get-aoc-map-rows-len m))
          (+ res (cal (append cache (list op)))))
      
      (if (every (lambda (s) (string= s " ")) row)
          
          (progn (incf res (cal (append cache (list op))))
                 (setf cache nil))
          
          (let ((option-op (car (last row)))) ;; <= maybe has operation, maybe not
            (if (string/= option-op " ")
                (setf op option-op))
            (push (apply #'str:concat (butlast row)) cache))))))

