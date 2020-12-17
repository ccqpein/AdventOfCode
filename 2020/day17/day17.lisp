(load "../tools.lisp")

(defparameter *r* 8)
(defparameter *c* 8)
(defparameter init-array (make-array (list *r* *c* 1) :initial-element #\.))

(loop
  with aa = (make-matrix-from-aoc
             (read-file-by-line "./day17.input"))
  for (rn cn) = (array-dimensions aa)
  for r from 0 to (1- *r*)
  do (loop
       for c from 0 to (1- *c*)
       do (setf (aref init-array r c 0) (aref aa r c))
       ))

(defun biger-m (m)
  (let* ((dims (array-dimensions m))
         (xn (car dims))
         (yn (cadr dims))
         (zn (caddr dims))
         (re-m (make-array (list (+ xn 2) (+ yn 2) (+ zn 2))
                           :initial-element #\.)))
    (loop
      for x from 0 below xn
      do (loop
           for y from 0 below yn
           do (loop for z from 0 below zn
                    do (setf (aref re-m (1+ x) (1+ y) (1+ z))
                             (aref m x y z)))))
    re-m))

(defun biger-m-2 (m)
  (let* ((dims (array-dimensions m))
         (xn (car dims))
         (yn (cadr dims))
         (zn (caddr dims))
         (wn (cadddr dims))
         (re-m (make-array (list (+ xn 2) (+ yn 2) (+ zn 2) (+ wn 2))
                           :initial-element #\.)))
    (loop
      for x from 0 below xn
      do (loop
           for y from 0 below yn
           do (loop for z from 0 below zn
                    do (loop for w from 0 below wn
                             do (setf (aref re-m (1+ x) (1+ y) (1+ z) (1+ w))
                                      (aref m x y z w))))))
    re-m))

(defun change-this-cube (m x y z xn yn zn)
  (let* ((all-possible (remove (list x y z)
                               (loop
                                 with re = '()
                                 for a from (1- x) to (1+ x)
                                 when (and (< a xn) (>= a 0))
                                   do (loop
                                        for b from (1- y) to (1+ y)
                                        when (and (< b yn) (>= b 0))
                                          do (loop
                                               for c from (1- z) to (1+ z)
                                               when (and (< c zn) (>= c 0))
                                                 do (push (list a b c) re)))
                                 finally (return re))
                               :test 'equal))
         (ac-num (loop for p in all-possible count (equal #\# (apply #'aref m p)))))
    
    (if (equal #\# (aref m x y z))
        (if (not (or (= 2 ac-num) (= 3 ac-num)))
            ;;(setf (aref m x y z) #\.)
            #\.
            #\#
            )
        (if (= 3 ac-num)
            ;;(setf (aref m x y z) #\#)
            #\#
            #\.
            ))
    ))

(defun change-this-cube-2 (m x y z w xn yn zn wn)
  (let* ((all-possible (remove (list x y z w)
                               (loop
                                 with re = '()
                                 for a from (1- x) to (1+ x)
                                 when (and (< a xn) (>= a 0))
                                   do (loop
                                        for b from (1- y) to (1+ y)
                                        when (and (< b yn) (>= b 0))
                                          do (loop
                                               for c from (1- z) to (1+ z)
                                               when (and (< c zn) (>= c 0))
                                                 do (loop
                                                      for d from (1- w) to (1+ w)
                                                      when (and (< d wn) (>= d 0))
                                                        do (push (list a b c d) re))))
                                 finally (return re))
                               :test 'equal))
         (ac-num (loop for p in all-possible count (equal #\# (apply #'aref m p)))))
    
    (if (equal #\# (aref m x y z w))
        (if (not (or (= 2 ac-num) (= 3 ac-num)))
            #\.
            #\#
            )
        (if (= 3 ac-num)
            #\#
            #\.
            ))
    ))

(defun change-all (m)
  (let* ((dims (array-dimensions m))
         (xn (car dims))
         (yn (cadr dims))
         (zn (caddr dims))
         (new-m (make-array dims :initial-element #\.)))
    (loop for x from 0 below xn
          do (loop for y from 0 below yn
                   do (loop for z from 0 below zn
                            do (setf (aref new-m x y z)
                                     (change-this-cube m x y z xn yn zn)))))
    new-m
    ))

(defun change-all-2 (m)
  (let* ((dims (array-dimensions m))
         (xn (car dims))
         (yn (cadr dims))
         (zn (caddr dims))
         (wn (cadddr dims))
         (new-m (make-array dims :initial-element #\.)))
    (loop for x from 0 below xn
          do (loop for y from 0 below yn
                   do (loop for z from 0 below zn
                            do (loop for w from 0 below wn
                                     do (setf (aref new-m x y z w)
                                              (change-this-cube-2 m x y z w xn yn zn wn))))))
    new-m
    ))

(defun print-z (m z)
  (let* ((dims (array-dimensions m))
         (xn (car dims))
         (yn (cadr dims)))
    (loop for x from 0 below xn
          do (loop for y from 0 below yn
                   do (format t "~a" (aref m x y z))
                   )
          do (format t "~%"))))

(defun all-sharp (m)
  (let* ((dims (array-dimensions m))
         (xn (car dims))
         (yn (cadr dims))
         (zn (caddr dims)))
    (loop
      with count = 0
      for x from 0 below xn
          do (loop for y from 0 below yn
                   do (loop for z from 0 below zn
                            if (equal #\# (aref m x y z))
                              do (incf count)))
      finally (return count))))

(defun all-sharp-2 (m)
  (let* ((dims (array-dimensions m))
         (xn (car dims))
         (yn (cadr dims))
         (zn (caddr dims))
         (wn (cadddr dims)))
    (loop
      with count = 0
      for x from 0 below xn
      do (loop for y from 0 below yn
               do (loop for z from 0 below zn
                        do (loop for w from 0 below wn
                                 if (equal #\# (aref m x y z w))
                                   do(incf count))))
      finally (return count))))

(defun part1 ()
  (dotimes (i 6)
    (setf init-array (biger-m init-array))
    (setf init-array (change-all init-array)))
  )

(defun part2 ()
  (let ((m (make-array (list *r* *c* 1 1) :initial-element #\.))
        )
    
    (loop
      with aa = (make-matrix-from-aoc
                 (read-file-by-line "./day17.input"))
      for (rn cn) = (array-dimensions aa)
      for r from 0 to (1- *r*)
      do (loop
           for c from 0 to (1- *c*)
           do (setf (aref m r c 0 0) (aref aa r c))
           ))
    
    (dotimes (i 6)
      (format t "~a" (array-dimensions m))
      (setf m (biger-m-2 m))
      (setf m (change-all-2 m)))
    
    (all-sharp-2 m)
    ))
