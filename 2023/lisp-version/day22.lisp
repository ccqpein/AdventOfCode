(load "../../tools/tools.lisp")

(defparameter *input* (read-file-by-line "../inputs/day22.input"))
(defparameter *input-demo* (read-file-by-line "../inputs/day22_demo.input"))

(defun parse-input (input)
  (let ((z-table (make-hash-table :test 'equal)))
    (loop for line in input
          for num upfrom 1
          do (let* ((ll (str:split "~" line))
                    (a (mapcar #'parse-integer (str:split "," (car ll))))
                    (b (mapcar #'parse-integer (str:split "," (cadr ll)))))
               
               (if (not (gethash (car (last a)) z-table))
                   (setf (gethash (car (last a)) z-table) (make-hash-table :test 'equal)))
               
               (if (not (gethash (car (last b)) z-table))
                   (setf (gethash (car (last b)) z-table) (make-hash-table :test 'equal)))
               
               (insert-z-table a b z-table num)
               ))
    (loop for z from 1 to (apply #'max (alexandria:hash-table-keys z-table))
          unless (gethash z z-table)
            do (setf (gethash z z-table) (make-hash-table :test 'equal)))
    z-table))

(defun insert-z-table (coop1 coop2 table v)
  (cond ((/= (nth 0 coop1) (nth 0 coop2))
         (setf (gethash (list v 1) (gethash (nth 2 coop1) table))
               (loop for a from (min (nth 0 coop1) (nth 0 coop2)) to (max (nth 0 coop1) (nth 0 coop2))
                     collect (list a (nth 1 coop1))))
         )
        ((/= (nth 1 coop1) (nth 1 coop2)) ;;y
         (setf (gethash (list v 1) (gethash (nth 2 coop1) table))
               (loop for a from (min (nth 1 coop1) (nth 1 coop2)) to (max (nth 1 coop1) (nth 1 coop2))
                     collect (list (nth 0 coop1) a)))
         )
        (t ;;z
         (loop for z from (min (nth 2 coop1) (nth 2 coop2)) to (max (nth 2 coop1) (nth 2 coop2))
               unless (gethash z table)
                 do (setf (gethash z table) (make-hash-table :test 'equal)))
         (loop for len from (1+ (abs (- (nth 2 coop1) (nth 2 coop2)))) downto 1
               for z from (min (nth 2 coop1) (nth 2 coop2)) to (max (nth 2 coop1) (nth 2 coop2))
               do (setf (gethash (list v len) (gethash z table)) (list (subseq coop1 0 2)))))))

(defun print-map (z-table)
  (loop for z in (sort (alexandria:hash-table-keys z-table) #'<)
        for layer = (gethash z z-table)
        if layer
          do (format t "z: ~a~%" z)
          and do (loop for coop being the hash-keys of layer
                         using (hash-value v)
                       do (format t "~a: ~a~%" coop v))
        else
          return nil
        ))

(defun check-lower (z-table z coops)
  (loop for z-lowest from (1- z) downto 1
        for all-coops = (loop for xx in (alexandria:hash-table-values (gethash z-lowest z-table)) append xx)
        when (intersection all-coops
                           coops
                           :test 'equal)
          return (1+ z-lowest)
        finally (return 1)))

(defun skyfall (z-table)
  (loop for z from 2 to (apply #'max (alexandria:hash-table-keys z-table))
        for z-layer = (gethash z z-table)
        when z-layer
          do (loop for vv being the hash-keys of z-layer
                     using (hash-value coops)
                   do (let ((lz (check-lower z-table z coops)))
                        ;;(format t "z: ~a, coops: ~a, lz: ~a~%" z coops lz)
                        (setf (gethash vv (gethash lz z-table)) coops)
                        (if (/= lz z) (remhash vv (gethash z z-table))))
                   )))

(defun try-to-dis (table zl zh cache-table)
  (move-one-by-one (alexandria:hash-table-values (gethash zl table))
                   (alexandria:hash-table-values (if (gethash zh table)
                                                     (gethash zh table)
                                                     (make-hash-table)))
                   zl zh cache-table))

(defun move-one-by-one (low-level high-level zl zh cache-table)
  (loop for one in low-level
        for rest = (loop for x in (remove one low-level :test #'equal) append x)
        ;;do (format t "one: ~a, rest: ~a~%" one rest)
        if (loop for uup in high-level
                 if (intersection uup rest :test #'equal)
                   collect t into aa
                 else
                   collect nil into aa
                 finally (return (or (not high-level)
                                     (and aa (every (lambda (x) x) aa)))))
          sum 1
          and do (format t "this one: ~a~%" one)
        else
          do (push one (gethash zl cache-table))
          and do (format t "this one crush: ~a~%" one)))


(defun day22 (input)
  (let ((z-table (parse-input input)))
    (skyfall z-table)
    (loop for z from 1 to (apply #'max (alexandria:hash-table-keys z-table))
          sum (try-to-dis z-table z (1+ z)
                          (make-hash-table) ;; usless in part1
                          ))))

;; (defun can-be-fall (table ll h)
;;   ;;(format t "ll:~a~%" ll)
;;   (let ((z-layer (gethash h table)))
;;     (if (not z-layer) (return-from can-be-fall 0))
;;     (loop
;;       with comp = 0
;;       for (num len) being the hash-keys of z-layer
;;         using (hash-value hl)
;;       if (not (intersection hl ll :test #'equal))
;;         collect hl into crush
;;         and do (if (> len 1) (incf comp))
;;       ;;do (format t "crush: ~a, hl: ~a~%" crush hl)
;;       finally (return (if (not crush)
;;                           0
;;                           (+ (length crush)
;;                              (- comp)
;;                              (can-be-fall table (remove-ele-in-l1-from-l2 crush hl) (1+ h)))))
;;       )))

;; (defun remove-ele-in-l1-from-l2 (l1 l2)
;;   (remove-if (lambda (e) (member e l1 :test 'equal)) l2))

;; (defun day22-2 (input)
;;   (let ((z-table (parse-input input))
;;         (record-table (make-hash-table :test #'equal)))
;;     (skyfall z-table)
;;     (loop for z from 1 to (apply #'max (alexandria:hash-table-keys z-table))
;;           do (try-to-dis z-table z (1+ z) record-table))

;;     (loop for z from 1 below (apply #'max (alexandria:hash-table-keys z-table))
;;           for crushes = (gethash z record-table)
;;           for z-level = (loop for x in (alexandria:hash-table-values (gethash z z-table)) append x)
;;           do (format t "crushes: ~a, z-level: ~a, z: ~a~%" crushes z-level z)
;;           sum (loop for c in crushes
;;                     sum (can-be-fall z-table
;;                                      (remove-ele-in-l1-from-l2 c z-level)
;;                                      (1+ z))))
;;     ))
