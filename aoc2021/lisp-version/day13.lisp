(load "../../2020/tools.lisp")

(defun parse-points-input (points)
  (mapcar (lambda (x)
            (mapcar 'parse-integer
                    (str:split "," x)))
          points))

(defun parse-folds (folds)
  (loop for l in folds
        collect (let ((a (str:split "=" (car (last (str:split " " l))))))
                  (list (car a) (parse-integer (cadr a))))))

(defun fold-this-points (point fold)
  (let ((x (car point))
        (y (cadr point))
        (f (car fold))
        (fv (cadr fold)))
    (cond ((string= "y" f)
           (cond ((> y fv) (list x (- fv (- y fv))))
                 ((= y fv) nil)
                 (t (list x y))))
          ((string= "x" f)
           (cond ((> x fv) (list (- fv (- x fv)) y))
                 ((= x fv) nil)
                 (t (list x y)))))))

(defun day13 (path)
  (let* ((content (read-file-by-line path))
         (empty-line-position (position "" content :test 'string=))
         )
    (multiple-value-bind (points folds)
        (values (parse-points-input (subseq content 0 empty-line-position))
                (parse-folds (subseq content (1+ empty-line-position))))
      (let* ((new-points (loop
                           for fold in folds
                           do (setf points
                                    (loop
                                      for p in points
                                      collect (fold-this-points p fold)))
                           do (format t "count of this round is: ~a~%"
                                      (set-count (make-hash-set-from-list
                                                  (remove nil points))))
                           finally (return points)))
             (x-max 0) (y-max 0)
             (points-set (make-hash-set-from-list new-points)))
        
        (loop for (x y) in points
              do (setf x-max (max x-max x)
                       y-max (max y-max y)))

        (loop for y from 0 to y-max
              do (loop for x from 0 to x-max
                       do (if (set-get points-set (list x y))
                              (format t "#")
                              (format t ".")))
              do (format t "~%"))
        ))))
