(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day9.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day9_demo.input"))

(defun parse-input (input)
  (loop for line in input
        collect (mapcar #'parse-integer
                        (cl-ppcre:all-matches-as-strings "-?\\d+" line))))

(defun gen-diff (l)
  (loop for (a b) on l when b collect (- b a)))

(defun handle (line &optional part2)
  (do* ((this-level line)
        (diff (gen-diff this-level) (gen-diff this-level))
        cache)
       ((every #'zerop diff)
        (if part2
            (reduce (lambda (a b) (- b a)) cache :initial-value (car this-level))
            (apply #'+ (append cache (last this-level)))))
    
    (if (every #'zerop diff)
        (setf this-level diff)
        (setf cache (cons (if part2
                              (car this-level)
                              (car (last this-level)))
                          cache)
              this-level diff))))

(defun day9 (input &optional part2)
  (let ((input (parse-input input)))
    (loop for line in input
          sum (handle line part2))))
