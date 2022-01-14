(load "../../2020/tools.lisp")

(defun parse-input (l)
  (loop
    with table = (make-hash-table :test 'equal)
    for x from 0 below (length l)
    do (loop
         for y from 0 below (length (car l))
         for this-char = (elt (elt l x) y)
         when (or (char= this-char #\>)
                  (char= this-char #\v))
           do (setf (gethash (list x y) table) this-char))
    finally (return table)))

(defun check-east (table x y x-max y-max)
  (if (= y y-max)
      (if (gethash (list x 0) table)
          nil
          (list x 0))
      (if (gethash (list x (1+ y)) table)
          nil
          (list x (1+ y)))))

(defun check-south (table x y x-max y-max)
  (if (= x x-max)
      (if (gethash (list 0 y) table)
          nil
          (list 0 y))
      (if (gethash (list (1+ x) y) table)
          nil
          (list (1+ x) y))))

(defun print-this-map (m x-max y-max)
  (loop for x from 0 to x-max
        do (loop for y from 0 to y-max
                 do (alexandria:if-let (c (gethash (list x y) m))
                      (format t (string c))
                      (format t ".")))
        do (format t "~%"))
  )

(defun part1 (path)
  (let* ((ll (read-file-by-line path))
         (map (parse-input ll))
         (x-max (1- (length ll)))
         (y-max (1- (length (car ll))))
         )
    ;;(format t "init: ~a~%" (print-this-map map x-max y-max))
    (loop for step from 0
          for new-map = (make-hash-table :test 'equal)
          for new-map-2 = (make-hash-table :test 'equal)
          do (progn
               (loop for kk being the hash-keys of map
                       using (hash-value c)
                     for (x y) = kk
                     if (char= c #\>)
                       do (alexandria:if-let (new-coorp (check-east map x y x-max y-max))
                            (setf (gethash new-coorp new-map) #\>)
                            (setf (gethash (list x y) new-map) #\>))
                     else
                       do (setf (gethash (list x y) new-map) #\v)
                     )

               ;;(print-this-map new-map x-max y-max)
               ;;(format t "------------~%")
               
               (loop for (x y) being the hash-keys of new-map
                       using (hash-value c)
                     if (char= c #\v)
                       do (alexandria:if-let (new-coorp (check-south new-map x y x-max y-max))
                            (setf (gethash new-coorp new-map-2) #\v)
                            (setf (gethash (list x y) new-map-2) #\v))
                     else
                       do (setf (gethash (list x y) new-map-2) #\>)
                     )

               ;;(print-this-map new-map-2 x-max y-max)
               ;;(format t "~%")

               ;; (if (= 4 step)
               ;;     (return-from part1 0))
               
               (if (equalp new-map-2 map)
                   (return-from part1 (1+ step))
                   (setf map new-map-2)))
          )))

;; (let ((a (make-hash-table :test 'equal))
;;       (b (make-hash-table :test 'equal)))
;;   (setf (gethash '1 a) 1)
;;   (setf (gethash '1 b) 1)
;;   (equalp a b))
