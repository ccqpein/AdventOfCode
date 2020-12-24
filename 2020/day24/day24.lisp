(load "../tools.lisp")

(defun parse-line (l)
  (do ((ll l)
       (result '()))
      ((string= "" ll) (reverse result))
      (cond ((str:starts-with? "s" ll)
             (progn
               (push (str:substring 0 2 ll) result)
               (setf ll (str:substring 2 t ll))))
            ((str:starts-with? "n" ll)
             (progn
               (push (str:substring 0 2 ll) result)
               (setf ll (str:substring 2 t ll))))
            (t
             (progn
               (push (str:substring 0 1 ll) result)
               (setf ll (str:substring 1 t ll)))))))

(defun coordinate (l)
  (let ((x 0)
        (y 0))
    (loop for w in l
          do (cond ((string= w "e")
                    (incf x 2))
                   ((string= w "w")
                    (decf x 2))
                   ((string= w "ne")
                    (incf x) (incf y))
                   ((string= w "nw")
                    (decf x) (incf y))
                   ((string= w "se")
                    (incf x) (decf y))
                   ((string= w "sw")
                    (decf x) (decf y))
                   ))
    (list x y)))

(defun part1 ()
  (let* ((input (read-file-by-line "./day24.input"))
         (l (mapcar #'parse-line input))
         (black '()))
    (loop
      for tile in l
      for coor = (coordinate tile)
      if (member coor black :test #'equal)
        do (delete coor black :test #'equal)
        ;;and do (print "back")
      else
        do (push coor black)
      finally (return (length (remove '(0 0) black :test #'equal)))))) 

(defun all-tiles (l)
  (let ((white '())
        (black '()))
    (loop
      for tile in l
      for coor = (coordinate tile)
      if (member coor black :test #'equal)
        do (delete coor black :test #'equal)
        and do (push coor white)
      else
        do (push coor black)
        and do (delete coor white :test #'equal))
    
    (values white (remove '(0 0) black :test #'equal))))

(defun expand-all-tiles (white black)
  (let ((xl 0)
        (xr 0)
        (yu 0)
        (yd 0))
    (dolist (ll (list white black))
      (loop
        for (x y) in ll
        do (cond ((< x xl) (setf xl x))
                 ((> x xr) (setf xr x)))
        do (cond ((> y yu) (setf yu y))
                 ((< y yd) (setf yd y)))))

    (loop
      with result = '()
      for x from (- xl 4) to (+ 4 xr) by 2
      do (loop for y from (- yd 2) to (+ 2 yu)
               if (oddp y)
                 do (push (list (1- x) y) result)
               else
                 do (push (list x y) result))
      finally (return (remove-duplicates result :test #'equal)))))

(defun around-this-tile (tile)
  (let ((x (car tile))
        (y (cadr tile)))
    (list (list (- x 2)
                y)
          (list (+ x 2)
                y)
          (list (+ x 1)
                (- y 1))
          (list (+ x 1)
                (+ y 1))
          (list (- x 1)
                (- y 1))
          (list (- x 1)
                (+ y 1)))))

(defun is-black (tile black)
  (member tile black :test #'equal))

(defun is-white (tile white)
  (member tile white :test #'equal))

(defun rule1 (tile black)
  (let* ((l (around-this-tile tile))
         (num (loop for tt in l
                    count (is-black tt black))))
    (if (or (= 0 num) (> num 2))
        0 ;; white
        1)))

(defun rule2 (tile black)
  (let* ((l (around-this-tile tile))
         (num (loop for tt in l
                    count (is-black tt black))))
    (if (= num 2)
        1 ;; black
        0)))


(let* ((tiles (multiple-value-list
               (all-tiles (mapcar #'parse-line (read-file-by-line "./day24.input")))))
       (black (nth 1 tiles))
       (white (nth 0 tiles))
       (all (expand-all-tiles black white))
       (new-black '())
       (new-white '()))
  ;;(format t "~a ~a ~a ~%" (length all) (length black) (length white))
  (format t "~a~%~a~%"  black white)
  (loop for tile in all
        if (is-black tile black)
          do (if (= 0 (rule1 tile black))
                 (push tile new-white)
                 (push tile new-black)
                 )
          and do (print tile)
        else
          do (if (= 1 (rule2 tile black))
                 (push tile new-black)
                 (push tile new-white)))
  
  (pprint new-black))

(let* ((tiles (multiple-value-list
               (all-tiles (mapcar #'parse-line (read-file-by-line "./day24.input")))))
       (black (nth 1 tiles))
       (white (nth 0 tiles))
       (all (expand-all-tiles black white))
       (new-black '())
       (new-white '()))

  (dotimes (i 2)
    (loop for tile in all
          if (is-black tile black)
            do (if (= 0 (rule1 tile black))
                   (push tile new-white)
                   (push tile new-black)
                   )
          else
            do (if (= 1 (rule2 tile black))
                   (push tile new-black)
                   (push tile new-white)))

    (setf black new-black
          white new-white
          all (expand-all-tiles black white)
          new-black '()
          new-white '()))
  (length black))

(let* ((tiles (multiple-value-list
               (all-tiles (mapcar #'parse-line (read-file-by-line "./day24.input")))))
       (black (nth 1 tiles))
       (white (nth 0 tiles))
       ;;(all (expand-all-tiles black white))
       (new-black '())
       ;;(new-white '())
       (check '()))

  (dotimes (i 1)
    (loop for tile in black
          do (push tile check)
          do (mapcar (lambda (x) (push x check))
                     (around-this-tile tile))
          finally (setf check (remove-duplicates check :test #'equal)))

    (print (length check))

    (loop for tile in check
          if (is-black tile black)
            do (if (= 1 (rule1 tile black))
                   (push tile new-black)
                   )
          else
            do (if (= 1 (rule2 tile black))
                   (push tile new-black)))

    (setf black (remove-duplicates new-black :test #'equal)
          new-black '()))
  
  (length black))
