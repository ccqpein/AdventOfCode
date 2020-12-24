(defun take (n a)
  (loop for c from 0 below n for i in a collect i))

(defun part1 (input &key iter-time)
  (let* ((a input)
         (len (length a)))
    (setf (cdr (last a)) a)
    (do ((count 1 (1+ count)))
        ((= count (1+ iter-time))
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
;;; Part 2 below
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun recover (l &optional (size 9))
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

(defun part2 (input &key (size 9) (count 9) (iter-time 10))
  (let* ((index-l (make-big-index-l input size))
         (this (car input))
         (*largest* size))
    
    (dotimes (i iter-time)
      (one-move index-l this)
      (setf this (nth this index-l)))

    (recover index-l count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hash table version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-big-index-table (l size)
  (let ((ll (make-big-index-l l size))
        (re (make-hash-table)))
    (loop for ind from 1 to size
          for v in (cdr ll)
          do (setf (gethash ind re) v))
    re))

(defun one-move-table (table this)
  (let* ((next-3 (do ((count 0 (1+ count))
                      (n this (gethash n table))
                      (re '()))
                     ((= count 3) (reverse re))
                   (push (gethash n table) re)
                   ))
         target
         new-next)

    ;;(format t "next-3: ~a~%" next-3)
    (setf target
          (or (loop for i from (1- this) downto 1
                    unless (member i next-3)
                      do (return i)
                    finally (return nil))
              (loop for lar from *largest* downto 1
                    unless (member lar next-3)
                      do (return lar))))

    (setf new-next (gethash (gethash (gethash (gethash this table) table) table) table)
          (gethash (third next-3) table) (gethash target table)
          (gethash target table) (car next-3)
          (gethash this table) new-next)
    ))

(defun recover-table (table &key count)
  (let ((next 1))
    (loop
      with re = '(1)
      and cc = 0
      for v = (gethash next table)
      until (or (= v 1) (= cc count))
      do (push v re)
      do (setf next v)
      do (incf cc)
      finally (return (reverse re))
      )))

(defun part2-v2 (input &key (size 9) (count 9) (iter-time 10))
  (let* ((index-table (make-big-index-table input size))
         (this (car input))
         (*largest* size))
    
    (dotimes (i iter-time)
      (one-move-table index-table this)
      (setf this (gethash this index-table)))

    (recover-table index-table :count count)))

;;(one-move '(0 2 5 8 6 4 7 3 9 1) 3)
;;(one-move '(0 5 8 2 6 4 7 3 9 1) 2)

;; (loop for k being the hash-keys
;;         using (hash-value v) of (part2-v2 '(3 8 9 1 2 5 4 6 7))
;;       do (format t "k: ~a, v: ~a~%" k v))

;; (loop for k being the hash-keys
;;         using (hash-value v) of (make-big-index-table '(3 8 9 1 2 5 4 6 7) 9)
;;       do (format t "k: ~a, v: ~a~%" k v))

;; (loop
;;   with table = (make-big-index-table '(3 8 9 1 2 5 4 6 7) 9)
;;     initially (one-move-table table 3)
;;   for k being the hash-keys
;;     using (hash-value v) of table
;;   do (format t "k: ~a, v: ~a~%" k v))

;; (part1 '(3 8 9 1 2 5 4 6 7) :iter-time 10)
;; (part1 '(9 2 5 1 7 6 8 3 4) :iter-time 100)

;; (part2-v2 '(3 8 9 1 2 5 4 6 7) :iter-time 1)
;; (part2-v2 '(9 2 5 1 7 6 8 3 4) :iter-time 100)

;; (part2-v2 '(3 8 9 1 2 5 4 6 7) :size 1000000 :count 5 :iter-time 10000000)
;; (part2-v2 '(9 2 5 1 7 6 8 3 4) :size 1000000 :count 5 :iter-time 10000000)
