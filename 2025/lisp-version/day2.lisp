(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day2.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day2_demo.input"))

(defun parse-input (input)
  (loop for line in (str:split "," (car input))
        append (str:match line
             ((start "-" end)
              (loop for n from (parse-integer start) to (parse-integer end)
                    collect (format nil "~a" n))))))

(defun invalid-id (s)
  (if (not (zerop (mod (length s) 2))) (return-from invalid-id nil))
  (equal (subseq s 0 (/ (length s) 2))
         (subseq s (/ (length s) 2))))

(defun invalid-id-2 (s)
  (let ((len (length s)))
    (loop for n from 1 below len
          if (and (zerop (mod len n))
                  (let ((cutted (cut-them s n)))
                    (every (lambda (n) (equal n (car cutted))) cutted)))
            return t)))

(defun cut-them (s len)
  (loop with start = 0
        if (< start (length s))
          collect (subseq s start (+ start len)) into res
          and do (incf start len)
        else do (return res)))

(defun day2 (input &optional part2)
  (apply #'+ (mapcar #'parse-integer
                     (loop for n in (parse-input input)
                           if (funcall (if part2 #'invalid-id-2 #'invalid-id) n)
                             collect n))))
