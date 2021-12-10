(load "../../2020/tools.lisp")

(defun find-pair (c)
  (case c
    (#\( #\)) (#\) #\()
    (#\{ #\}) (#\} #\{)
    (#\< #\>) (#\> #\<)
    (#\[ #\]) (#\] #\[)))

(defun part1 (path)
  (let ((content (read-file-by-line path)))
    (labels ((point (c)
               (case c
                 (#\] 57) (#\) 3) (#\} 1197) (#\> 25137))))
      (loop for l in content
            for stack = '()
            sum (loop
                  with result = 0
                  for c across l
                  unless (case c
                           ((#\[ #\( #\{ #\<) (progn (push c stack) t))
                           ((#\] #\) #\} #\>) (if (char= (car stack) (find-pair c))
                                                  (progn (pop stack) t)
                                                  (progn (setf result (point c)) nil))))
                    return result
                  finally (return result)))))
  )

(defun part2 (path)
  (let ((content (read-file-by-line path)))
    (labels ((point (cs)
               (loop
                 with result = 0
                 for c in cs
                 do (setf result (* 5 result))
                 do (incf result
                          (case c
                            (#\[ 2) (#\( 1) (#\{ 3) (#\< 4)
                            (otherwise 0)
                            ))
                 finally (return result)
                 )))
      (let ((all-points
              (loop for l in content
                    for stack = '()
                    collect (loop
                              for c across l
                              unless (case c
                                       ((#\[ #\( #\{ #\<) (progn (push c stack) t))
                                       ((#\] #\) #\} #\>) (if (char= (car stack) (find-pair c))
                                                              (progn (pop stack) t)
                                                              nil)))
                                return nil
                              finally (return (point stack))))))
        (setf all-points (sort (remove nil all-points) #'>))
        (nth (floor (/ (length all-points) 2)) all-points)))))
