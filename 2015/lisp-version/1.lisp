(load "../../tools/tools.lisp")

(defun day1 ()
  (let ((input (read-file-by-line "../inputs/day1.input")))
    (loop for c across (nth 0 input)
          sum (if (char= #\( c) 1 -1))
    ))

(defun day1-2 ()
  (let ((input (read-file-by-line "../inputs/day1.input"))
        (sum 0))
    (loop for ind from 0
          for c across (nth 0 input)
          do (if (char= #\( c) (incf sum) (decf sum))
          if (< sum 0) return (1+ ind))
    ))

(defun day2 ()
  (let ((input (read-file-by-line "../inputs/day2.input")))
    (loop for l in input
          sum (str:match l
                ((l "x" w "x" h)
                 (let ((l (parse-integer l))
                       (w (parse-integer w))
                       (h (parse-integer h)))
                   (+ (* 2 l w) (* 2 w h) (* 2 h l)
                      (min (* l w) (* w h) (* h l)))))))
    ))

(defun day2-2 ()
  (let ((input (read-file-by-line "../inputs/day2.input")))
    (loop for l in input
          sum (str:match l
                ((l "x" w "x" h)
                 (let ((l (parse-integer l))
                       (w (parse-integer w))
                       (h (parse-integer h))
                       x)
                   (setf x (sort `(,l ,w ,h) #'<))
                   (+ (* 2 (nth 0 x)) (* 2 (nth 1 x)) (* (nth 0 x) (nth 1 x) (nth 2 x)))))
                ))))

(defun day3 ()
  (let ((input (nth 0 (read-file-by-line "../inputs/day3.input")))
        (set (make-hash-set))
        (x 0) (y 0))
    (set-insert set `(,x ,y))
    (loop for c across input
          do (case c
               (#\^ (incf y))
               (#\> (incf x))
               (#\< (decf x))
               (#\v (decf y)))
          do (set-insert set `(,x ,y))
          finally (return (set-count set)))
    ))

(defun day3-2 ()
  (let ((input (nth 0 (read-file-by-line "../inputs/day3.input")))
        (set (make-hash-set))
        (x0 0) (y0 0) (x1 0) (y1 0))
    (set-insert set `(,x0 ,y0))
    (loop for (a b) on (concatenate 'list input) by #'cddr
          do (case a
               (#\^ (incf y0))
               (#\> (incf x0))
               (#\< (decf x0))
               (#\v (decf y0)))
          do (set-insert set `(,x0 ,y0))

          do (case b
               (#\^ (incf y1))
               (#\> (incf x1))
               (#\< (decf x1))
               (#\v (decf y1)))
          do (set-insert set `(,x1 ,y1))
          finally (return (set-count set)))
    ))

(defun day4 (input &optional p2)
  (loop for n from 1
        if (string=
            (if p2 "000000" "00000")
            (str:substring 0 (if p2 6 5)
                           (format nil "~(~{~2,'0X~}~)"
                                   (concatenate 'list
                                                (sb-md5:md5sum-string (str:concat
                                                                       input
                                                                       (write-to-string n)))))))
          return n))
