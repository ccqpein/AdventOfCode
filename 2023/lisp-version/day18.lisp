(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day18.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day18_demo.input"))

(defun parse-input (input)
  (mapcar (lambda (l)
            (let ((s (str:split " " l)))
              (list (car s) (parse-integer (cadr s)))))
          input))

(defun draw-map2 (parsed-input)
  (let ((table (make-hash-table :test 'equal)))
    (setf (gethash '(0 0) table) "#")
    (loop with this = '(0 0)
          for l in parsed-input
          for (dir num) = (subseq l 0 2)
          do (cond 
               ((string= dir "R") (loop repeat num
                                        do (setf this (mapcar #'+ '(0 1) this))
                                        do (setf (gethash this table) "#")))
               ((string= dir "L") (loop repeat num
                                        do (setf this (mapcar #'+ '(0 -1) this))
                                        do (setf (gethash this table) "#")))
               ((string= dir "U") (loop repeat num
                                        do (setf this (mapcar #'+ '(-1 0) this))
                                        do (setf (gethash this table) "#")))
               ((string= dir "D") (loop repeat num
                                        do (setf this (mapcar #'+ '(1 0) this))
                                        do (setf (gethash this table) "#")))))
    table
    ))

(defun find-four-corner (table)
  (loop with row-min = 0
        and row-max = 0
        and col-min = 0
        and col-max = 0
        for (r c) in (alexandria:hash-table-keys table)
        do (if (< r row-min) (setf row-min r))
        do (if (> r row-max) (setf row-max r))
        do (if (< c col-min) (setf col-min c))
        do (if (> c col-max) (setf col-max c))
        finally (return (list row-min row-max col-min col-max))
        ))

(defun neighbour (coop)
  (loop for offset in '((0 1) (0 -1) (-1 -1) (-1 0) (-1 1)
                        (1 -1) (1 0) (1 1))
        collect (mapcar #'+ coop offset)))

(defun in-the-range (coop r-min r-max c-min c-max)
  (and (<= r-min (car coop) r-max)
       (<= c-min (cadr coop) c-max)))

(defun flood-fill (map r-min r-max c-min c-max)
  (let ((r-min (1- r-min))
        (c-min (1- c-min))
        (r-max (1+ r-max))
        (c-max (1+ c-max))
        (table (make-hash-table :test 'equal)))
    (setf (gethash (list r-min c-min) table) t)
    (loop with continue-flag = t
          and next-round = (list (list r-min c-min))
          
          while continue-flag
          do (setf continue-flag nil)
          do (loop for next in next-round
                   append (loop for n in (neighbour next)
                                if (and (in-the-range n r-min r-max c-min c-max)
                                        (not (gethash n map))
                                        (not (gethash n table)))
                                  do (setf (gethash n table) t
                                           continue-flag t)
                                  and collect n into aa
                                finally (return aa))
                     into nn
                   finally (setf next-round nn)
                   )
          )
    (- (* (- r-max r-min -1)
          (- c-max c-min -1))
       (length (alexandria:hash-table-keys table)))
    ))

(defun day18 (input)
  (let* ((parsed-input (parse-input input))
         (map (draw-map2 parsed-input)))
    (apply #'flood-fill map (find-four-corner map))
    ))

(defun parse-input2 (input)
  (mapcar (lambda (l)
            (let ((ins (car (last (str:split " " l))))
                  dir num)
              (setf num (parse-integer (subseq ins 2 (- (length ins) 2)) :radix 16)
                    dir (subseq ins (- (length ins) 2) (1- (length ins)))
                    dir (cond ((string= dir "0") "R")
                              ((string= dir "1") "D")
                              ((string= dir "2") "L")
                              ((string= dir "3") "U")))
              (list dir num)))
          input))

;;;;;;;;;;;;;;;;;;;; doesn't work ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun draw-map3 (parsed-input)
  (loop with this = '(0 0)
        and next
        for (dir num) in parsed-input
        collect (cond 
                  ((string= dir "R")
                   (setf next (mapcar #'+ this (list 0 num)))
                   (list this next))
                  ((string= dir "L")
                   (setf next (mapcar #'+ this (list 0 (- num))))
                   (list this next))
                  ((string= dir "U")
                   (setf next (mapcar #'+ this (list (- num) 0)))
                   (list this next))
                  ((string= dir "D")
                   (setf next (mapcar #'+ this (list num 0)))
                   (list this next)))
        do (setf this next)))

(defun find-four-corner2 (all-lines)
  (loop with row-min = 0
        and row-max = 0
        and col-min = 0
        and col-max = 0
        for line in all-lines
        do (loop for (r c) in line
                 do (if (< r row-min) (setf row-min r))
                 do (if (> r row-max) (setf row-max r))
                 do (if (< c col-min) (setf col-min c))
                 do (if (> c col-max) (setf col-max c)))
        finally (return (list row-min row-max col-min col-max))
        ))

(defun in-the-line (coop lines)
  (loop for (l1 l2) in lines
        do (if (= (car l1) (car l2))
               (if (and (= (car coop) (car l1))
                        (or (<= (cadr l1) (cadr coop) (cadr l2))
                            (>= (cadr l1) (cadr coop) (cadr l2))))
                   (return-from in-the-line t))
               (if (and (= (cadr coop) (cadr l1))
                        (or (<= (car l1) (car coop) (car l2))
                            (>= (car l1) (car coop) (car l2))))
                   (return-from in-the-line t)))))

(defun flood-fill2 (lines r-min r-max c-min c-max)
  (let ((r-min (1- r-min))
        (c-min (1- c-min))
        (r-max (1+ r-max))
        (c-max (1+ c-max))
        (table (make-hash-table :test 'equal)))
    (setf (gethash (list r-min c-min) table) t)
    (loop with continue-flag = t
          and next-round = (list (list r-min c-min))
          
          while continue-flag
          do (setf continue-flag nil)
          do (loop for next in next-round
                   append (loop for n in (neighbour next)
                                if (and (in-the-range n r-min r-max c-min c-max)
                                        (not (in-the-line n lines))
                                        (not (gethash n table)))
                                  do (setf (gethash n table) t
                                           continue-flag t)
                                  and collect n into aa
                                finally (return aa))
                     into nn
                   finally (setf next-round nn)
                   )
          )
    (- (* (- r-max r-min -1)
          (- c-max c-min -1))
       (length (alexandria:hash-table-keys table)))
    ))

;; (defun day18-2 (input)
;;   (let* ((parsed-input (parse-input2 input))
;;          (lines (draw-map3 parsed-input)))
;;     (apply #'flood-fill2 lines (find-four-corner2 lines))
;;     ))
;;;;;;;;;;;;;;;;;;;; doesn't work ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun draw-map4 (parsed-input)
  (loop with this = '(0 0)
        and result = '((0 0))
        for (dir num) in parsed-input
        do (push (progn (cond 
                          ((string= dir "R")
                           (setf this (mapcar #'+ this (list 0 num)))
                           )
                          ((string= dir "L")
                           (setf this (mapcar #'+ this (list 0 (- num))))
                           )
                          ((string= dir "U")
                           (setf this (mapcar #'+ this (list (- num) 0)))
                           )
                          ((string= dir "D")
                           (setf this (mapcar #'+ this (list num 0)))
                           ))
                        this)
                 result)
        finally (return (reverse result))))

(defun day18-2 (input &optional part2)
  (let* ((parsed-input (if part2 (parse-input2 input) (parse-input input)))
         (lines (draw-map4 parsed-input)))
    (+ 1
       (abs (shoelace lines)) ;; has 0,0 start and end already
       (/ (apply #'+ (mapcar #'cadr parsed-input)) 2))
    ))

;;(day18-2 *input*)
;;(day18-2 *input* t)

