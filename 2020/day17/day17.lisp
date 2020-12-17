(load "../tools.lisp")

(defparameter *r* 3)
(defparameter *c* 3)
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

;; (defun part1 ()
;;   (let ((dims (array-dimensions ))))
;;   )
