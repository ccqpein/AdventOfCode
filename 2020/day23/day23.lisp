(defun take (n a)
  (loop for c from 0 below n for i in a collect i))

(defun part1 (input)
  (let* ((a input)
         (len (length a)))
    (setf (cdr (last a)) a)
    (do ((count 1 (1+ count)))
        ((= count 101)
         (return (take len a)))

      (let ((this (car a))
            (next-3 (cdr a))
            des)
        
        (setf (cdr a) (cddddr a)
              des (find-destination a (- len 3) (1- this)))
        (loop
          for tt from 0 below (- len 3)
          do (setf a (cdr a))
          if (= des (car a))
            do (setf (cdddr next-3) (cdr a)
                     (cdr a) next-3))
        
        (do () ((= (car a) this))
          (setf a (cdr a)))
        (setf a (cdr a))

        ;; (format t "after move ~a: ~{~a ~}~%" count
        ;;         (take len a))
        ))))

(defun find-destination (l count target)
  (let ((largest 0)
        (smallest target))
    (let ((a (loop for c from 0 below count
                   for i in l
                   do (if (> i largest) (setf largest i))
                   do (if (< i smallest) (setf smallest i))
                   collect i)))
      (or (loop for x from target downto smallest
                if (member x a)
                  do (return x)
                finally (return nil))
          largest)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *largest* 9)

(defun one-move (l this)
  (let* ((next-3 (do ((count 0 (1+ count))
                      (n this (nth n l))
                      (re '()))
                     ((= count 3) (reverse re))
                   (push (nth n l) re)
                   ))
         target
         new-next)
    
    (setf target
          (or (loop for i from (1- this) downto 1
                    unless (member i next-3)
                      do (return i)
                    finally (return nil))
              (loop for lar from *largest* downto 1
                    unless (member lar next-3)
                      do (return lar))))

    (setf new-next (nth (nth (nth (nth this l) l) l) l)
          (nth (third next-3) l) (nth target l)
          (nth target l) (car next-3)
          (nth this l) new-next)
    ))

(defun recover (l &optional (size (length l)))
  (append '(1)
          (loop
            with i = 1
            for ti from 1 below size
            collect (nth i l) into cache
            do (setf i (nth i l))
            finally (return cache))))

(defun test ()
  (let ((a ;;'(0 2 5 8 6 4 7 3 9 1)
          (make-big-index-l '(3 8 9 1 2 5 4 6 7) 10)
          )
        (this 3))
    
    (dotimes (i 10)
      (one-move a this)
      (setf this (nth this a))
      )
    
    ;;(pprint a)
    ;;(pprint (recover a))
    (butlast (cdr (recover a)))))

(defun make-big-index-l (l size)
  (do ((re (make-list (1+ size) :initial-element 0))
       (a (car l) (car rest))
       (rest (cdr l) (cdr rest)))
      
      ((not rest)
       (if  (> size (length l))
            (progn
              (setf (nth a re) 10) ;; 
              (loop
                for ind from 10 to size
                do (setf (nth ind re) (1+ ind)))
              (setf (car (last re)) (car l)))
            (setf (nth a re) (car l)))
       re)
    
    (setf (nth a re) (car rest))))

(defun part2 ()
  (let* ((input ;;'(9 2 5 1 7 6 8 3 4)
           '(3 8 9 1 2 5 4 6 7)
           )
         (index-l (make-big-index-l input 1000000))
         (this 3))
    
    (dotimes (i 10000000)
      (one-move index-l this)
      (setf this (nth this index-l)))

    (recover index-l 4)))

(defun part2-v2 ()
  (let* ((input ;;'(9 2 5 1 7 6 8 3 4)
           '(3 8 9 1 2 5 4 6 7)
           )
         (index-l (make-big-index-l input 1000000))
         (this 3))
    
    (dotimes (i 10000000)
      (one-move index-l this)
      (setf this (nth this index-l)))

    (recover index-l 4)))

;;(one-move '(0 2 5 8 6 4 7 3 9 1) 3)
;;(one-move '(0 5 8 2 6 4 7 3 9 1) 2)

