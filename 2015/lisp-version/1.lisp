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

(defun day5-day9 ()
  (error "it is mess, I am too lazy."))

(defun day10 (input time)
  (labels ((helper2 (input len last bucket)
             (if (null input)
                 (append bucket (list `,(write-to-string len) `,(string last)))
                 (if (string= (nth 0 input) last)
                     (helper2 (cdr input) (1+ len) last bucket)
                     (helper2 (cdr input) 1 (nth 0 input) (append bucket (list `,(write-to-string len) `,(string last))))
                     ))))    
    (let ((input (concatenate 'list input)))
      (length (dotimes (a time input)
                (setf input (helper2 (cdr input) 1 (nth 0 input) '()))
                ;;(setf input (helper input))
                )))))

;; (day10 "1113122113" 40)
;; (day10 "1113122113" 50)

(defun day11-day15 ()
  (error "it is mess, I am too lazy."))

(defun day16 (&optional part2)
  (let ((checklist-table (loop with table = (make-hash-table :test 'equal)
                               for l in '("children: 3"
                                          "cats: 7"
                                          "samoyeds: 2"
                                          "pomeranians: 3"
                                          "akitas: 0"
                                          "vizslas: 0"
                                          "goldfish: 5"
                                          "trees: 3"
                                          "cars: 2"
                                          "perfumes: 1")
                               do (str:match l
                                    ((item ": " num) (setf (gethash item table) (parse-integer num))))
                               finally (return table))))
    (labels ((p1 (table sue item0 num0 item1 num1 item2 num2)
               (if (and (= (gethash item0 table) (parse-integer num0))
                        (= (gethash item1 table) (parse-integer num1))
                        (= (gethash item2 table) (parse-integer num2)))
                   (parse-integer sue)))
             (p2 (table sue item0 num0 item1 num1 item2 num2)
               (loop for (item num) on (list item0 num0 item1 num1 item2 num2) by #'cddr
                     do (str:match item
                          (("cats") (if (>= (gethash item table) (parse-integer num))
                                        (return-from p2 nil)))
                          (("trees") (if (>= (gethash item table) (parse-integer num))
                                         (return-from p2 nil)))
                          (("pomeranians") (if (<= (gethash item table) (parse-integer num))
                                               (return-from p2 nil)))
                          (("goldfish") (if (<= (gethash item table) (parse-integer num))
                                            (return-from p2 nil)))
                          (t (if (/= (gethash item table) (parse-integer num))
                                 (return-from p2 nil))))
                     finally (return (parse-integer sue)))))
      (let ((input (read-file-by-line "../inputs/day16.input")))
        (loop for l in input
              do (str:match l
                   (("Sue " sue ": " item0 ": " num0 ", " item1 ": " num1 ", " item2 ": " num2)
                    (let ((x (funcall (if part2 #'p2 #'p1)
                                      checklist-table sue item0 num0 item1 num1 item2 num2)))
                      (if x (return-from day16 x))))))))))

(defun day17 (&optional part2)
  (labels ((p1 (l target)
             (if (= 0 target)
                 1
                 (loop for ind from 0 below (length l)
                       for x = (nth ind l)
                       if (<= x target)
                         sum (p1 (subseq l (1+ ind)) (- target x)) into all
                       finally (return all))))
           (p2 (l target deep set)
             (if (= 0 target)
                 (progn (if (not (gethash deep set))
                            (setf (gethash deep set) 1)
                            (incf (gethash deep set)))
                        1)
                 (loop for ind from 0 below (length l)
                       for x = (nth ind l)
                       if (<= x target)
                         sum (p2 (subseq l (1+ ind)) (- target x) (1+ deep) set) into all
                       finally (return all))
                 )))
    (let ((input (sort (loop for l in (read-file-by-line "../inputs/day17.input") collect (parse-integer l))
                       #'>)))
      (if (not part2)
          (p1 input 150)
          (let ((set (make-hash-table :test 'equal)))
            (p2 input 150 1 set)
            (gethash (car (sort (alexandria:hash-table-keys set) #'<)) set))))))
