(load "../../2020/tools.lisp")

(defun day6 (path day)
  (let* ((content (mapcar #'parse-integer
                          (str:split #\, (car (read-file-by-line path)))))
         (record (loop
                   for i from 0 to 8
                   collect (length (remove-if-not (lambda (e) (= i e)) content)))))
    (loop
      for d from 1 to day
      for zero = (nth 0 record)
      do (loop for i from 0 to 7 do (setf (nth i record) (nth (1+ i) record)))
      do (incf (nth 6 record) zero)
      do (setf (nth 8 record) zero))
    (apply #'+ record)))
